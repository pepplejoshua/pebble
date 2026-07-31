package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

// Result is the immutable result of 06b validation.
type Result struct {
	successful   bool
	solution     *infer.Solution
	records      *solvedRecords
	requirements map[symbol.SymbolID][]Requirement
	ir           *tir.Unit
	expressions  map[symbol.SyntaxRef]ExpressionResult
	places       map[symbol.SyntaxRef]PlaceResult
	conversions  map[symbol.SyntaxRef]ConversionResult
	calls        map[symbol.SyntaxRef]CallResult
	members      map[symbol.SyntaxRef]MemberResult
	controls     map[symbol.SyntaxRef]ControlResult
}

func (r *Result) Expression(ref symbol.SyntaxRef) (ExpressionResult, bool) {
	if r == nil {
		return ExpressionResult{}, false
	}
	v, ok := r.expressions[ref]
	return v, ok
}
func (r *Result) Place(ref symbol.SyntaxRef) (PlaceResult, bool) {
	if r == nil {
		return PlaceResult{}, false
	}
	v, ok := r.places[ref]
	v.Projections = append([]PlaceProjectionResult(nil), v.Projections...)
	return v, ok
}
func (r *Result) Conversion(ref symbol.SyntaxRef) (ConversionResult, bool) {
	if r == nil {
		return ConversionResult{}, false
	}
	v, ok := r.conversions[ref]
	return v, ok
}
func (r *Result) Call(ref symbol.SyntaxRef) (CallResult, bool) {
	if r == nil {
		return CallResult{}, false
	}
	v, ok := r.calls[ref]
	v.Arguments = append([]ConversionResult(nil), v.Arguments...)
	return v, ok
}
func (r *Result) Member(ref symbol.SyntaxRef) (MemberResult, bool) {
	if r == nil {
		return MemberResult{}, false
	}
	v, ok := r.members[ref]
	return v, ok
}
func (r *Result) Control(ref symbol.SyntaxRef) (ControlResult, bool) {
	if r == nil {
		return ControlResult{}, false
	}
	v, ok := r.controls[ref]
	v.Exits = append([]ExitResult(nil), v.Exits...)
	v.Defers = append([]tir.NodeID(nil), v.Defers...)
	return v, ok
}

func (r *Result) Successful() bool {
	return r != nil && r.successful
}

func (r *Result) Solution() *infer.Solution {
	if r == nil {
		return nil
	}
	return r.solution
}

func (r *Result) SymbolType(id symbol.SymbolID) (infer.TypeResult, bool) {
	if r == nil || r.solution == nil {
		return infer.TypeResult{}, false
	}
	return r.solution.SymbolType(id)
}

func (r *Result) Requirements(owner symbol.SymbolID) []Requirement {
	if r == nil {
		return nil
	}
	return append([]Requirement(nil), r.requirements[owner]...)
}

func (r *Result) Instantiation(ref symbol.SyntaxRef) (infer.Instantiation, bool) {
	if r == nil || r.solution == nil {
		return infer.Instantiation{}, false
	}
	return r.solution.Instantiation(ref)
}

// IR returns the closed typed-IR unit, or nil when publication is gated off.
// IR() is non-nil exactly when the whole checker succeeded: no earlier-phase
// errors, a successful solve, every retained record resolved and validated,
// structural control/global/context/generic-body/entry validation passed, and
// typed-IR construction with its closed verification succeeded. run06b builds
// the unit as its final step and stores it only on a fully successful result,
// so a failed result always carries nil IR and a successful result always
// carries non-nil IR.
func (r *Result) IR() *tir.Unit {
	if r == nil {
		return nil
	}
	return r.ir
}

// run06b is the package-private entry point for 06b validation. It runs every
// validator in the spec's validation order, then typed-IR construction and
// closed verification via buildUnit as the final gate. Every step fails the
// whole result; IR is stored only on a fully successful result.
func run06b(handoff *solveHandoff, diagnostics *diagnostic.DiagnosticSet, config Config) *Result {
	config = normalizeConfig(config)

	if !auditHandoff(handoff, diagnostics, config) {
		return &Result{successful: false}
	}

	records, ok := resolveRecords(handoff, diagnostics, config)
	if !ok {
		return &Result{successful: false}
	}

	// Declarations, binding forms, globals, and callable declarations.
	if !validateBindings(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateSizeof(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateCallableRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}

	// Members, aggregates, calls, brackets, indices, slices, and context flow.
	if !validateMemberRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateAggregateRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateCallRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateIndexRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateContextFlowRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	// Operators, casts, places, assignments, and compatibility.
	if !validateArithmeticOperators(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateBooleanOperators(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validatePlaceRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateAssignmentRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateCompatibilityRecords(handoff, records, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}

	// Generic requirements.
	requirements, ok := validateRequirements(handoff, records, diagnostics, config)
	if !ok {
		return newResult(handoff, records, nil, nil, false)
	}

	// Per-function structural control flow, defers, returns, and reachability.
	if !auditControlArena(handoff, diagnostics, config) {
		return newResult(handoff, records, nil, nil, false)
	}
	if !validateControlFlow(handoff, records, diagnostics, config) {
		return newResult(handoff, records, requirements, nil, false)
	}
	if !validateSwitches(handoff, records, diagnostics, config) {
		return newResult(handoff, records, requirements, nil, false)
	}
	if !validateDefers(handoff, records, diagnostics, config) {
		return newResult(handoff, records, requirements, nil, false)
	}

	// Configured entry point.
	if !validateEntry(handoff, records, requirements, diagnostics, config) {
		return newResult(handoff, records, requirements, nil, false)
	}

	// Typed-IR construction and closed verification are the last step, gated
	// so IR exists only when every earlier validation step succeeded. buildUnit
	// returns ok == false for a generation that already had errors or for any
	// IR construction/verification failure; either way the whole result fails
	// and no unit is published.
	unit, ok := buildUnit(handoff, records, requirements, config)
	if !ok || unit == nil {
		return newResult(handoff, records, requirements, nil, false)
	}

	return newResult(handoff, records, requirements, unit, true)
}
