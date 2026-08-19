package infer

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
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

// TestErrorTaintPropagatesThroughReceiverNotNominal asserts that when a method
// call fails because the receiver is not a nominal type, the call's own result
// terms (the callable and the member result) inherit the error taint rather
// than each reporting a fresh T0510. This pins the receiverNominal wiring into
// the error-taint mechanism: the root T0507 remains, but no dependent cell
// cascades into its own unresolved diagnostic.
func TestErrorTaintPropagatesThroughReceiverNotNominal(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	receiver := session.Variable(Origin{Role: "receiver"})
	session.Add(Equal(receiver, session.Known(store.Builtins().Int), Origin{Role: "receiver is int"}))
	callable := session.Variable(Origin{Role: "callable"})
	result := session.Variable(Origin{Role: "result"})
	site := symbol.SyntaxRef{Module: 1, Node: 1}
	session.Add(CallMember(receiver, "something", callable, nil, result, nil, site, Origin{Role: "method call"}))
	session.PublishSyntax(site, result)
	session.Solve()

	if !hasDiagnostic(diagnostics, CodeCapability) {
		t.Fatalf("root capability failure must be reported: %+v", diagnostics.Items())
	}
	if hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("callable/result must inherit taint, not report T0510: %+v", diagnostics.Items())
	}
}

// TestErrorTaintReceiverNotNominalDoesNotSuppressIndependentErrors is the
// independence check for the receiver-not-nominal family: a second method call
// on an independent, non-tainted receiver still reports its own T0507. Only
// cells that structurally depend on the first failed receiver are suppressed.
func TestErrorTaintReceiverNotNominalDoesNotSuppressIndependentErrors(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	receiverA := session.Variable(Origin{Role: "receiver a"})
	session.Add(Equal(receiverA, session.Known(store.Builtins().Int), Origin{Role: "a is int"}))
	callableA := session.Variable(Origin{Role: "callable a"})
	resultA := session.Variable(Origin{Role: "result a"})
	siteA := symbol.SyntaxRef{Module: 1, Node: 1}
	session.Add(CallMember(receiverA, "something", callableA, nil, resultA, nil, siteA, Origin{Role: "method call a"}))
	session.PublishSyntax(siteA, resultA)

	receiverB := session.Variable(Origin{Role: "receiver b"})
	session.Add(Equal(receiverB, session.Known(store.Builtins().I64), Origin{Role: "b is i64"}))
	callableB := session.Variable(Origin{Role: "callable b"})
	resultB := session.Variable(Origin{Role: "result b"})
	siteB := symbol.SyntaxRef{Module: 1, Node: 2}
	session.Add(CallMember(receiverB, "something", callableB, nil, resultB, nil, siteB, Origin{Role: "method call b"}))
	session.PublishSyntax(siteB, resultB)
	session.Solve()

	if countDiagnostics(diagnostics, CodeCapability) != 2 {
		t.Fatalf("both independent capability failures must be reported: %+v", diagnostics.Items())
	}
	if countDiagnostics(diagnostics, CodeUnresolved) != 0 {
		t.Fatalf("no dependent T0510 cascade expected: %+v", diagnostics.Items())
	}
}

// TestErrorTaintPropagatesThroughHasFieldNoMember asserts that a field access
// whose member name does not exist on a nominal type reports the root T0507
// (with its did-you-mean suggestion) but taints the field-access result term
// rather than leaving it unresolved as a fresh T0510. This pins the hasField
// wiring into the error-taint mechanism: the field cell is a fresh cell created
// by the same constraint that is failing, so tainting the receiver alone would
// not suppress its cascade.
func TestErrorTaintPropagatesThroughHasFieldNoMember(t *testing.T) {
	program, store, diagnostics, session, field := nominalFieldFixture(t, "cout")
	session.Add(HasField(session.Known(mustNominal(program, store, "Box")), "cout", field, Origin{Role: "member access"}))
	session.PublishSymbol(1, field)
	session.Solve()

	if !hasDiagnostic(diagnostics, CodeCapability) {
		t.Fatalf("root capability failure must be reported: %+v", diagnostics.Items())
	}
	if hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("field term must inherit taint, not report T0510: %+v", diagnostics.Items())
	}
	if !suggestionMentioned(diagnostics, "count") {
		t.Fatalf("did-you-mean suggestion must be preserved: %+v", diagnostics.Items())
	}
}

// TestErrorTaintHasFieldNoMemberDoesNotSuppressIndependentErrors is the
// independence check for the hasField no-such-member family: a second, unrelated
// type error in the same file still reports independently. Only the field term
// structurally depending on the failed member access is suppressed.
func TestErrorTaintHasFieldNoMemberDoesNotSuppressIndependentErrors(t *testing.T) {
	program, store, diagnostics, session, field := nominalFieldFixture(t, "cout")
	session.Add(HasField(session.Known(mustNominal(program, store, "Box")), "cout", field, Origin{Role: "member access"}))
	session.PublishSymbol(1, field)

	a := session.Variable(Origin{Role: "a"})
	session.Add(Equal(a, session.Known(store.Builtins().Int), Origin{Role: "a is int"}))
	session.Add(Equal(a, session.Known(store.Builtins().Bool), Origin{Role: "a is bool (conflict)"}))
	session.PublishSymbol(2, a)
	session.Solve()

	if !hasDiagnostic(diagnostics, CodeCapability) || !hasDiagnostic(diagnostics, CodeUnification) {
		t.Fatalf("both independent errors must be reported: %+v", diagnostics.Items())
	}
	if countDiagnostics(diagnostics, CodeUnresolved) != 0 {
		t.Fatalf("no dependent T0510 cascade expected: %+v", diagnostics.Items())
	}
}

// nominalFieldFixture prepares a program with a Box struct, the session, and a
// fresh field variable for a HasField constraint on a nominal Box receiver.
func nominalFieldFixture(t *testing.T, name string) (*Program, *types.Store, *diagnostic.DiagnosticSet, *Session, Term) {
	t.Helper()
	program, pd := prepareSource(t, []byte(`type Box = struct { count i32; };
fn main() int {
  var b = Box.{ count = 5 };
  return b.count;
}`))
	if pd.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", pd.Items())
	}
	store := program.inputs.Types
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	field := session.Variable(Origin{Role: name})
	return program, store, diagnostics, session, field
}

// mustNominal returns the interned nominal TypeID for a source type name.
func mustNominal(program *Program, store *types.Store, name string) types.TypeID {
	var decl symbol.SymbolID
	for _, s := range program.inputs.Resolution.Symbols.All() {
		if s.Kind == symbol.SymbolType && s.Name == name {
			decl = s.ID
			break
		}
	}
	if decl == 0 {
		panic("nominal type not found: " + name)
	}
	id, err := store.Intern(types.NominalKey(decl, []types.TypeID{store.Builtins().Int}))
	if err != nil {
		panic(err)
	}
	return id
}

// suggestionMentioned reports whether any diagnostic message contains name.
func suggestionMentioned(set *diagnostic.DiagnosticSet, name string) bool {
	for _, item := range set.Items() {
		if strings.Contains(item.Message, name) {
			return true
		}
	}
	return false
}
