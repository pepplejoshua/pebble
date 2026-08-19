package infer

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

// These tests pin the error-taint propagation that stops diagnostic cascades.
// When a single root mistake taints one inference cell (a T0505/T0507-style
// unify or capability failure), every cell that structurally depends on it is
// left unresolved and would otherwise each report its own T0510. The solver
// must forward the taint to those dependent cells instead of letting them fan
// out into separate diagnostics, while still reporting genuinely independent
// errors independently.

// TestErrorTaintPropagatesThroughEqual asserts that a variable bound to an
// already-tainted cell (via an equality constraint) inherits the taint and is
// not reported as a fresh T0510 cascade. The root T0505 remains.
func TestErrorTaintPropagatesThroughEqual(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	a := session.Variable(Origin{Role: "a"})
	b := session.Variable(Origin{Role: "b"})
	session.Add(Equal(a, session.Known(store.Builtins().Int), Origin{Role: "a is int"}))
	session.Add(Equal(a, session.Known(store.Builtins().Bool), Origin{Role: "a is bool (conflict)"}))
	session.Add(Equal(b, a, Origin{Role: "b depends on a"}))
	session.PublishSymbol(1, b)
	session.Solve()

	if !hasDiagnostic(diagnostics, CodeUnification) {
		t.Fatalf("root unify failure must be reported: %+v", diagnostics.Items())
	}
	if hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("dependent cell b must inherit taint, not report T0510: %+v", diagnostics.Items())
	}
}

// TestErrorTaintPropagatesThroughHasField asserts that the result term of a
// member access on an already-tainted receiver inherits the taint rather than
// being reported as a fresh T0510. The root T0505 remains.
func TestErrorTaintPropagatesThroughHasField(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	a := session.Variable(Origin{Role: "a"})
	session.Add(Equal(a, session.Known(store.Builtins().Int), Origin{Role: "a is int"}))
	session.Add(Equal(a, session.Known(store.Builtins().Bool), Origin{Role: "a is bool (conflict)"}))
	field := session.Variable(Origin{Role: "field"})
	session.Add(HasField(a, "x", field, Origin{Role: "member access"}))
	session.PublishSymbol(1, field)
	session.Solve()

	if !hasDiagnostic(diagnostics, CodeUnification) {
		t.Fatalf("root unify failure must be reported: %+v", diagnostics.Items())
	}
	if hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("field term must inherit taint, not report T0510: %+v", diagnostics.Items())
	}
}

// TestErrorTaintDoesNotSuppressIndependentErrors is the safety check: two
// genuinely unrelated mistakes in cells that share no constraint must each be
// reported. The propagation only follows structural dependency within a single
// constraint, so a second, independent conflict is never swallowed.
func TestErrorTaintDoesNotSuppressIndependentErrors(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	a := session.Variable(Origin{Role: "a"})
	b := session.Variable(Origin{Role: "b"})
	session.Add(Equal(a, session.Known(store.Builtins().Int), Origin{Role: "a is int"}))
	session.Add(Equal(a, session.Known(store.Builtins().Bool), Origin{Role: "a is bool (conflict)"}))
	session.Add(Equal(b, a, Origin{Role: "b depends on a"}))

	c := session.Variable(Origin{Role: "c"})
	d := session.Variable(Origin{Role: "d"})
	session.Add(Equal(c, session.Known(store.Builtins().I64), Origin{Role: "c is i64"}))
	session.Add(Equal(c, session.Known(store.Builtins().Str), Origin{Role: "c is str (conflict)"}))
	session.Add(Equal(d, c, Origin{Role: "d depends on c"}))

	session.PublishSymbol(1, b)
	session.PublishSymbol(2, d)
	session.Solve()

	if countDiagnostics(diagnostics, CodeUnification) != 2 {
		t.Fatalf("both independent root failures must be reported: %+v", diagnostics.Items())
	}
	if countDiagnostics(diagnostics, CodeUnresolved) != 0 {
		t.Fatalf("no dependent T0510 cascade expected: %+v", diagnostics.Items())
	}
}

func countDiagnostics(set *diagnostic.DiagnosticSet, code diagnostic.Code) int {
	n := 0
	for _, item := range set.Items() {
		if item.Code == code {
			n++
		}
	}
	return n
}
