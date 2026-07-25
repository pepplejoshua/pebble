package infer

import (
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

func TestFatalAddChoiceLimitsSkipRecoverySolving(t *testing.T) {
	for _, test := range []struct {
		name    string
		config  Config
		prepare func(*Session, Term) ConstraintID
		message string
	}{
		{
			name:   "constraint limit",
			config: Config{MaxConstraints: 1},
			prepare: func(session *Session, subject Term) ConstraintID {
				id, _ := session.AddChoice(testChoice(session, subject, Origin{Span: source.NewSpan(1, 2, 4), Role: "rejected choice"}))
				return id
			},
			message: "constraint limit of 1 exceeded",
		},
		{
			name:   "choice limit",
			config: Config{MaxChoices: 1},
			prepare: func(session *Session, subject Term) ConstraintID {
				first, _ := session.AddChoice(testChoice(session, subject, Origin{Role: "retained choice"}))
				if first == 0 {
					t.Fatal("first choice was rejected")
				}
				id, _ := session.AddChoice(testChoice(session, subject, Origin{Span: source.NewSpan(1, 2, 4), Role: "rejected choice"}))
				return id
			},
			message: "choice limit of 1 exceeded",
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, _ := testProgram(t)
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, test.config)
			unresolved := session.Variable(Origin{Role: "otherwise unresolved"})
			literal := session.IntegerLiteral([]byte("42"), Origin{Role: "must not default"})
			if id := test.prepare(session, unresolved); id != 0 {
				t.Fatalf("over-limit choice received id %d", id)
			}

			solution := session.Solve()
			assertFatalRecovery(t, session, solution, diagnostics, diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeResourceLimit,
				Message:  test.message,
				Primary:  diagnostic.Label{Span: source.NewSpan(1, 2, 4), Message: "rejected choice"},
			})
			assertNotDefaulted(t, session, literal)
		})
	}
}

func TestFatalGuardedPublicationLimitsStayUnpublished(t *testing.T) {
	program, _ := testProgram(t)

	t.Run("slot", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxSolvedSlots: 1})
		selector := session.Variable(Origin{Role: "selector"})
		choiceID, choice := session.AddChoice(testChoice(session, selector, Origin{Role: "choice"}))
		first := session.PublishGuardedSlot(choice, 0, session.Variable(Origin{Role: "first guarded slot"}))
		if first == (SlotID{}) {
			t.Fatal("first guarded slot was rejected")
		}
		if second := session.PublishGuardedSlot(choice, 1, session.Variable(Origin{Role: "second guarded slot"})); second != (SlotID{}) {
			t.Fatal("over-limit guarded slot was accepted")
		}

		solution := session.Solve()
		assertFatalRecovery(t, session, solution, diagnostics, diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeResourceLimit,
			Message:  "solved slot limit of 1 exceeded",
		})
		if _, ok := solution.Selection(choiceID); ok {
			t.Fatal("fatal slot recovery published a choice selection")
		}
		if _, ok := solution.Slot(first); ok || len(solution.Slots()) != 0 {
			t.Fatal("fatal slot recovery published an inactive guarded slot")
		}
	})

	t.Run("instantiation", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxShapeComponents: 1})
		selector := session.Variable(Origin{Role: "selector"})
		choiceID, choice := session.AddChoice(testChoice(session, selector, Origin{Role: "choice"}))
		firstSite := symbol.SyntaxRef{Module: 1, Node: 1}
		secondSite := symbol.SyntaxRef{Module: 1, Node: 2}
		session.PublishGuardedInstantiation(choice, 0, firstSite, 50, nil)
		session.PublishGuardedInstantiation(choice, 1, secondSite, 50, nil)

		solution := session.Solve()
		assertFatalRecovery(t, session, solution, diagnostics, diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeResourceLimit,
			Message:  "generic instantiation publication limit of 1 exceeded",
			Primary:  diagnostic.Label{Message: ""},
		})
		if _, ok := solution.Selection(choiceID); ok {
			t.Fatal("fatal instantiation recovery published a choice selection")
		}
		if _, ok := solution.Instantiation(firstSite); ok {
			t.Fatal("fatal instantiation recovery published an inactive guarded instantiation")
		}
		if _, ok := solution.Instantiation(secondSite); ok {
			t.Fatal("rejected guarded instantiation appeared in the solution")
		}
	})
}

func TestFatalDuringOrdinarySolvingStopsRecoveryPhases(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{MaxUnificationSteps: 1})
	origin := Origin{Span: source.NewSpan(2, 3, 8), Role: "bounded equality"}
	shape := session.Variable(Origin{Role: "ready shape"})
	literal := session.IntegerLiteral([]byte("42"), Origin{Role: "must not default"})
	session.Add(ConstrainShape(shape, PointerShape(Leaf(session.Known(store.Builtins().Int))), Origin{}))
	session.Add(Equal(session.Known(store.Builtins().Int), session.Known(store.Builtins().Int), Origin{}))
	session.Add(Equal(session.Known(store.Builtins().Bool), session.Known(store.Builtins().Bool), origin))

	solution := session.Solve()
	assertFatalRecovery(t, session, solution, diagnostics, diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeResourceLimit,
		Message:  "unification step limit of 1 exceeded",
		Primary:  diagnostic.Label{Span: origin.Span, Message: origin.Role},
	})
	root := session.find(shape.id)
	if root == 0 || session.cells[root-1].shape == nil || session.cells[root-1].known != 0 {
		t.Fatalf("ready shape materialized after fatal recovery: root=%d cell=%+v", root, session.cells[root-1])
	}
	assertNotDefaulted(t, session, literal)
}

func TestFatalDuringChoiceExplorationIsOrderIndependent(t *testing.T) {
	program, _ := testProgram(t)
	choiceOrigin := Origin{Span: source.NewSpan(3, 5, 13), Role: "bounded choice"}
	var baseline diagnostic.Diagnostic
	for _, reverse := range []bool{false, true} {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxChoiceStates: 1})
		selector := session.Variable(Origin{Role: "selector"})
		literal := session.IntegerLiteral([]byte("42"), Origin{Role: "must not default"})
		alternatives := []Alternative{
			{Label: "int", Constraints: []Constraint{Equal(selector, session.Known(program.inputs.Types.Builtins().Int), Origin{})}},
			{Label: "bool", Constraints: []Constraint{Equal(selector, session.Known(program.inputs.Types.Builtins().Bool), Origin{})}},
		}
		if reverse {
			alternatives[0], alternatives[1] = alternatives[1], alternatives[0]
		}
		choiceID, choice := session.AddChoice(OneOf(alternatives, choiceOrigin))
		slot := session.PublishGuardedSlot(choice, 0, literal)
		site := symbol.SyntaxRef{Module: 1, Node: 3}
		session.PublishGuardedInstantiation(choice, 1, site, 50, []Term{literal})

		solution := session.Solve()
		expected := diagnostic.Diagnostic{
			Severity: diagnostic.Error,
			Code:     CodeResourceLimit,
			Message:  "choice state limit of 1 exceeded",
			Primary:  diagnostic.Label{Span: choiceOrigin.Span, Message: choiceOrigin.Role},
		}
		assertFatalRecovery(t, session, solution, diagnostics, expected)
		assertNotDefaulted(t, session, literal)
		if _, ok := solution.Selection(choiceID); ok {
			t.Fatalf("reverse=%v fatal exploration published a choice selection", reverse)
		}
		if _, ok := solution.Slot(slot); ok || len(solution.Slots()) != 0 {
			t.Fatalf("reverse=%v fatal exploration published a guarded slot", reverse)
		}
		if _, ok := solution.Instantiation(site); ok {
			t.Fatalf("reverse=%v fatal exploration published a guarded instantiation", reverse)
		}
		if reverse && !reflect.DeepEqual(diagnostics.Items()[0], baseline) {
			t.Fatalf("reversed exploration changed diagnostic\nforward: %#v\nreverse: %#v", baseline, diagnostics.Items()[0])
		}
		baseline = diagnostics.Items()[0]
	}
}

func TestFatalAfterChoiceSelectionClearsGuardedPublications(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{MaxConstraintRequeues: 3})
	selector := session.Variable(Origin{Role: "selector"})
	session.Add(Equal(selector, session.Known(store.Builtins().Bool), Origin{}))
	requeueOrigin := Origin{Span: source.NewSpan(4, 8, 12), Role: "post-choice callable"}
	session.Add(Callable(session.Variable(Origin{}), nil, session.Variable(Origin{}), requeueOrigin))
	literal := session.IntegerLiteral([]byte("42"), Origin{Role: "must not default"})
	choiceID, choice := session.AddChoice(OneOf([]Alternative{
		{Label: "rejected", Constraints: []Constraint{Equal(selector, session.Known(store.Builtins().Int), Origin{})}},
		{Label: "selected", Constraints: []Constraint{Equal(selector, session.Known(store.Builtins().Bool), Origin{})}},
	}, Origin{Role: "choice"}))
	slot := session.PublishGuardedSlot(choice, 1, literal)
	site := symbol.SyntaxRef{Module: 1, Node: 4}
	session.PublishGuardedInstantiation(choice, 1, site, 50, []Term{literal})

	solution := session.Solve()
	assertFatalRecovery(t, session, solution, diagnostics, diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeResourceLimit,
		Message:  "constraint requeue limit of 3 exceeded",
		Primary:  diagnostic.Label{Span: requeueOrigin.Span, Message: requeueOrigin.Role},
	})
	assertNotDefaulted(t, session, literal)
	if _, ok := solution.Selection(choiceID); ok {
		t.Fatal("post-selection fatal recovery retained the choice selection")
	}
	if _, ok := solution.Slot(slot); ok || len(solution.Slots()) != 0 {
		t.Fatal("post-selection fatal recovery retained the guarded slot")
	}
	if _, ok := solution.Instantiation(site); ok {
		t.Fatal("post-selection fatal recovery retained the guarded instantiation")
	}
}

func testChoice(session *Session, subject Term, origin Origin) Constraint {
	return OneOf([]Alternative{
		{Label: "int", Constraints: []Constraint{Equal(subject, session.Known(session.program.inputs.Types.Builtins().Int), Origin{})}},
		{Label: "bool", Constraints: []Constraint{Equal(subject, session.Known(session.program.inputs.Types.Builtins().Bool), Origin{})}},
	}, origin)
}

func assertFatalRecovery(t *testing.T, session *Session, solution *Solution, diagnostics *diagnostic.DiagnosticSet, expected diagnostic.Diagnostic) {
	t.Helper()
	if solution == nil || !solution.finalized || solution.Successful() {
		t.Fatalf("solution=%+v", solution)
	}
	items := diagnostics.Items()
	if len(items) != 1 || !reflect.DeepEqual(items[0], expected) {
		t.Fatalf("diagnostics\n got: %#v\nwant: %#v", items, expected)
	}
	if hasDiagnostic(diagnostics, CodeUnresolved) || hasDiagnostic(diagnostics, CodeInvalidType) || hasDiagnostic(diagnostics, CodeAmbiguous) {
		t.Fatalf("fatal recovery emitted a derived diagnostic: %+v", items)
	}
	if len(solution.selections) != 0 {
		t.Fatalf("fatal recovery selections=%+v", solution.selections)
	}
	// Repeated reads exercise the finalized recovery tables without relying on
	// caller order or mutable session state.
	if !reflect.DeepEqual(solution.Slots(), solution.Slots()) || !reflect.DeepEqual(solution.SymbolTypes(), solution.SymbolTypes()) || !reflect.DeepEqual(solution.SyntaxTypes(), solution.SyntaxTypes()) {
		t.Fatal("fatal recovery queries are nondeterministic")
	}
	if !session.fatal {
		t.Fatal("T0512 did not leave the session fatal")
	}
}

func assertNotDefaulted(t *testing.T, session *Session, term Term) {
	t.Helper()
	root := session.find(term.id)
	if root == 0 || session.cells[root-1].known != 0 {
		t.Fatalf("term defaulted after fatal recovery: root=%d cell=%+v", root, session.cells[root-1])
	}
}
