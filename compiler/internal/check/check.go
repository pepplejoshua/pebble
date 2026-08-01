package check

import (
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

// Check is the public checker entry point for one compilation. It runs 06a
// semantic fact generation and 06b validation/typed-IR construction in
// sequence and returns the unified immutable Result. Diagnostics are
// caller-supplied and both phases append to the same set, matching the
// established run06a/run06b calling convention.
//
// Exactly one traversal and one solve happen per call: run06a performs the
// single syntax traversal and calls Solve exactly once, and the resulting
// handoff is consumed exactly once by run06b. Check introduces no second call
// path into either phase.
func Check(inputs Inputs, diagnostics *diagnostic.DiagnosticSet, config Config) *Result {
	if diagnostics == nil {
		diagnostics = diagnostic.NewDiagnosticSet()
	}
	handoff := run06a(inputs, diagnostics, config)
	return run06b(handoff, diagnostics, config, inputs.Types)
}
