package check

import (
	"fmt"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

// auditHandoff validates the solve handoff for 06b before any record is resolved.
// No handoff field owns an AST-bearing or mutable object — this is structurally
// guaranteed by the type system (frozenCompilation, frozenRecords, frozenRoots,
// frozenConstants cannot hold AST nodes by construction).
func auditHandoff(handoff *solveHandoff, diagnostics *diagnostic.DiagnosticSet, config Config) bool {
	reporter := newValidationReporter(diagnostics, config.MaxDiagnostics)

	fail := func(message string) bool {
		reporter.add(diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeGeneration,
			Message:  message,
		})
		reporter.flush()
		return false
	}

	if handoff.GenerationHadErrors && handoff.Semantics == nil {
		return false
	}

	if handoff.Semantics == nil || handoff.Solution == nil {
		return fail("semantic snapshot or solution is nil")
	}

	if !handoff.Semantics.Matches(handoff.Solution) {
		return fail("semantic snapshot does not match the solution")
	}

	if handoff.Semantics.Types() == nil {
		return fail("semantic snapshot has no type snapshot")
	}

	// Matches already guarantees Types().Len() == Solution.storeLength, so
	// for any handoff that reaches this point, every legitimately-solved
	// TypeID is already within range - the check below is not redundant
	// with Matches, though: it also catches a TypeFinal result carrying a
	// zero/invalid TypeID (a hypothetical solver inconsistency Matches's
	// own aggregate length comparison would not notice, since it only
	// compares counts, not individual entries).
	for _, entry := range handoff.Solution.SymbolTypes() {
		if entry.Result.State == infer.TypeFinal && !handoff.Semantics.Types().Contains(entry.Result.Type) {
			return fail("type snapshot does not contain a symbol type result")
		}
	}
	for _, entry := range handoff.Solution.SyntaxTypes() {
		if entry.Result.State == infer.TypeFinal && !handoff.Semantics.Types().Contains(entry.Result.Type) {
			return fail("type snapshot does not contain a syntax type result")
		}
	}
	for _, entry := range handoff.Solution.Slots() {
		if entry.Result.State == infer.TypeFinal && !handoff.Semantics.Types().Contains(entry.Result.Type) {
			return fail("type snapshot does not contain a slot type result")
		}
	}

	return true
}

type validationReporter struct {
	budget *validationDiagnosticBudget
}

func newValidationReporter(set *diagnostic.DiagnosticSet, max uint32) *validationReporter {
	return &validationReporter{budget: newValidationDiagnosticBudget(set, max)}
}

func (r *validationReporter) add(value diagnostic.Diagnostic) {
	if r != nil {
		r.budget.add(value)
	}
}

func (r *validationReporter) flush() {}

type validationDiagnosticBudget struct {
	set           *diagnostic.DiagnosticSet
	max           uint32
	count         uint32
	lastIndex     int
	lastPrimary   diagnostic.Label
	hasDiagnostic bool
	overflow      bool
}

func newValidationDiagnosticBudget(set *diagnostic.DiagnosticSet, max uint32) *validationDiagnosticBudget {
	if set == nil {
		set = diagnostic.NewDiagnosticSet()
	}
	return &validationDiagnosticBudget{set: set, max: max, lastIndex: -1}
}

func (b *validationDiagnosticBudget) add(value diagnostic.Diagnostic) bool {
	if b == nil || b.overflow {
		return false
	}
	if b.count >= b.max {
		if b.hasDiagnostic {
			b.set.Replace(b.lastIndex, diagnostic.Diagnostic{
				Severity: diagnostic.Error, Code: CodeGeneration,
				Message: fmt.Sprintf("validation diagnostic limit of %d reached", b.max),
				Primary: b.lastPrimary,
			})
		}
		b.overflow = true
		return false
	}
	b.lastIndex = b.set.Len()
	b.lastPrimary = value.Primary
	b.set.Add(value)
	b.hasDiagnostic = true
	b.count++
	return true
}
