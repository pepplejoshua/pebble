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

// IR returns the typed-IR unit. It always returns nil at this point in the
// project — real typed-IR construction is a much later slice's job (06b.7b),
// and none of the six conditions the spec lists for a non-nil IR() (structural
// control/global/context/generic-body/entry validation, typed-IR construction
// and verification) exist yet.
func (r *Result) IR() *tir.Unit {
	return nil
}

// run06b is the package-private entry point for 06b validation. It runs the
// two steps that exist at this point (auditHandoff and resolveRecords), then
// returns a Result. Later slices will extend this function to run the
// remaining validation-order steps (declarations, members/calls/brackets,
// operators/places/compatibility, structural control
// flow, entry point, typed-IR construction) before finalizing Result — this
// function is deliberately incomplete right now, not broken.
func run06b(handoff *solveHandoff, diagnostics *diagnostic.DiagnosticSet, config Config) *Result {
	config = normalizeConfig(config)

	if !auditHandoff(handoff, diagnostics, config) {
		return &Result{successful: false}
	}

	records, ok := resolveRecords(handoff, diagnostics, config)
	if !ok {
		return &Result{successful: false}
	}
	requirements, ok := validateRequirements(handoff, records, diagnostics, config)
	if !ok {
		return &Result{successful: false}
	}

	return &Result{
		successful:   true,
		solution:     handoff.Solution,
		records:      records,
		requirements: requirements,
	}
}
