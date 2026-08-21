package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const CodePlace diagnostic.Code = "C0606"

type placeWritabilityResult uint8

type addressOperatorsBySyntax map[symbol.SyntaxRef]bool

const (
	placeWritable placeWritabilityResult = iota + 1
	placeNotWritable
	placeWritabilityUnresolved
	placeStringIndex
)

// placeWritability returns whether a place is writable. Unresolved projections
// are distinct from non-writable places so validation can suppress them;
// string indexes remain reportable because they are never places.
func placeWritability(handoff *solveHandoff, records *solvedRecords, place *placeRecord, addressOperators addressOperatorsBySyntax) placeWritabilityResult {
	if handoff == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil || place == nil {
		return placeWritabilityUnresolved
	}
	// Function parameters are mutable storage even though they are not `var`
	// binding declarations. This remains scoped to the root binding and does
	// not make immutable locals writable.
	typeSnapshot := handoff.Semantics.Types()
	writable := place.RootMutable || place.RootKind == symbol.SymbolParameter
	if !writable {
		// A local initialized from an address-of expression is an alias, not
		// the storage being mutated. Its field/index projections inherit the
		// pointee's writability even when the pointer binding itself is `let`.
		for _, retained := range handoff.Records.Records() {
			if retained.Binding == nil || retained.Binding.Symbol != place.Root || !retained.Binding.InitializerPresent {
				continue
			}
			if result, ok := records.Root(retained.Binding.Initializer); ok && result.State == infer.TypeFinal {
				if key, ok := typeSnapshot.Key(result.Type); ok && key.Kind() == types.Pointer {
					writable = true
				}
			}
			writable = writable || addressOperators[retained.Binding.InitializerSyntax]
			break
		}
	}
	for _, projection := range place.Projections {
		switch projection.Kind {
		case placeStorage:
			writable = writable || place.RootMutable || place.RootKind == symbol.SymbolParameter
		case placeDereference:
			writable = true
		case placeField, placeTuple:
			// These projections preserve the base place's mutability.
		case placeIndex:
			result, ok := records.Root(projection.Base)
			if !ok || result.State != infer.TypeFinal {
				return placeWritabilityUnresolved
			}
			key, ok := typeSnapshot.Key(result.Type)
			if !ok {
				return placeWritabilityUnresolved
			}
			if key.Kind() == types.Builtin {
				builtin, _ := key.Builtin()
				if builtin == types.Str {
					return placeStringIndex
				}
			}
			writable = key.Kind() == types.Slice || writable && key.Kind() == types.Array
		}
	}
	if writable {
		return placeWritable
	}
	return placeNotWritable
}

func placeIsWritable(handoff *solveHandoff, records *solvedRecords, place *placeRecord) bool {
	return placeWritability(handoff, records, place, nil) == placeWritable
}

func validatePlaceRecords(handoff *solveHandoff, records *solvedRecords, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	if handoff == nil || handoff.Solution == nil || handoff.Semantics == nil || handoff.Semantics.Types() == nil || records == nil {
		return true
	}
	reporter := newValidationReporter(diagnostics, normalizeConfig(config).MaxDiagnostics)
	failed := false
	// No activeOperatorRecord filter: the original inner scan this replaces
	// (inside placeWritability, before this precompute existed) matched by
	// syntax ref and operator family alone, with no active-record check --
	// adding one here would silently drop address-of operators tied to a
	// record activeOperatorRecord excludes (an unselected guarded
	// alternative), which the original scan would still have found.
	addressOperators := make(addressOperatorsBySyntax)
	for _, retained := range handoff.Records.Records() {
		if retained.Operator != nil && retained.Operator.Family == operatorAddress {
			addressOperators[retained.Header.Syntax] = true
		}
	}
	type operatorKey struct {
		syntaxRef symbol.SyntaxRef
		owner     symbol.SymbolID
	}
	operators := make(map[operatorKey]operatorFamily)
	// SyntaxRef and Owner are the stable join key shared by the place and
	// operator records retained for one expression.
	for _, retained := range handoff.Records.Records() {
		if retained.Operator == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		operators[operatorKey{retained.Header.Syntax, retained.Header.Owner}] = retained.Operator.Family
	}
	report := func(header recordHeader) {
		failed = true
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodePlace,
			Message:  "place is not writable",
			Primary:  diagnostic.Label{Span: header.Span},
		})
	}
	for _, retained := range handoff.Records.Records() {
		place := retained.Place
		if place == nil || !activeOperatorRecord(handoff, retained.Header) {
			continue
		}
		writability := placeWritability(handoff, records, place, addressOperators)
		if writability == placeWritabilityUnresolved {
			continue
		}
		if writability == placeStringIndex {
			report(retained.Header)
			continue
		}
		family, matched := operators[operatorKey{retained.Header.Syntax, retained.Header.Owner}]
		if matched && family == operatorAddress && writability == placeNotWritable {
			report(retained.Header)
		}
	}
	return !failed
}
