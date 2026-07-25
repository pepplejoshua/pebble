package infer

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestGuardedInstantiationSelectedAndDefensivelyCopied(t *testing.T) {
	program, store := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	selector := session.Variable(Origin{})
	session.Add(Equal(selector, session.Known(store.Builtins().Bool), Origin{}))
	argument := session.Variable(Origin{})
	choiceID, choice := session.AddChoice(OneOf([]Alternative{
		{Label: "generic", Constraints: []Constraint{Equal(argument, session.Known(store.Builtins().Char), Origin{})}},
		{Label: "index", Constraints: []Constraint{Equal(selector, session.Known(store.Builtins().Int), Origin{})}},
	}, Origin{}))
	site := symbol.SyntaxRef{Module: 1, Node: 2}
	arguments := []Term{argument}
	session.PublishGuardedInstantiation(choice, 0, site, 50, arguments)
	arguments[0] = session.Known(store.Builtins().Bool)

	solution := session.Solve()
	if selected, ok := solution.Selection(choiceID); !ok || selected != 0 {
		t.Fatalf("selection=(%d,%v)", selected, ok)
	}
	instantiation, ok := solution.Instantiation(site)
	if !solution.Successful() || !ok || instantiation.Generic != 50 || len(instantiation.Arguments) != 1 || instantiation.Arguments[0].Type != store.Builtins().Char {
		t.Fatalf("solution=%v instantiation=%+v", solution.Successful(), instantiation)
	}
	instantiation.Arguments[0] = TypeResult{State: TypeError}
	fresh, _ := solution.Instantiation(site)
	if fresh.Arguments[0].Type != store.Builtins().Char {
		t.Fatal("solution instantiation aliases caller storage")
	}
}

func TestGuardedInstantiationInactiveFailedAndAmbiguousStayInvisible(t *testing.T) {
	for _, test := range []struct {
		name         string
		alternatives func(*Session, Term, Term) []Alternative
		selected     bool
	}{
		{
			name: "inactive",
			alternatives: func(s *Session, selector, argument Term) []Alternative {
				return []Alternative{
					{Label: "generic", Constraints: []Constraint{Equal(selector, s.Known(s.program.inputs.Types.Builtins().Int), Origin{}), Equal(argument, s.Known(s.program.inputs.Types.Builtins().Char), Origin{})}},
					{Label: "index", Constraints: []Constraint{Equal(selector, s.Known(s.program.inputs.Types.Builtins().Bool), Origin{})}},
				}
			},
			selected: true,
		},
		{
			name: "failed",
			alternatives: func(s *Session, selector, argument Term) []Alternative {
				return []Alternative{
					{Label: "generic", Constraints: []Constraint{Equal(selector, s.Known(s.program.inputs.Types.Builtins().Int), Origin{}), Equal(argument, s.Known(s.program.inputs.Types.Builtins().Char), Origin{})}},
					{Label: "index", Constraints: []Constraint{Equal(selector, s.Known(s.program.inputs.Types.Builtins().Uint), Origin{})}},
				}
			},
		},
		{
			name: "ambiguous",
			alternatives: func(s *Session, _, argument Term) []Alternative {
				return []Alternative{
					{Label: "generic", Constraints: []Constraint{Numeric(argument, Origin{})}},
					{Label: "index", Constraints: []Constraint{Equal(s.Known(s.program.inputs.Types.Builtins().Bool), s.Known(s.program.inputs.Types.Builtins().Bool), Origin{})}},
				}
			},
		},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, store := testProgram(t)
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			selector := session.Known(store.Builtins().Bool)
			if test.name != "ambiguous" {
				selector = session.Variable(Origin{})
				session.Add(Equal(selector, session.Known(store.Builtins().Bool), Origin{}))
			}
			argument := session.IntegerLiteral([]byte("42"), Origin{})
			choiceID, choice := session.AddChoice(OneOf(test.alternatives(session, selector, argument), Origin{}))
			site := symbol.SyntaxRef{Module: 1, Node: 3}
			session.PublishGuardedInstantiation(choice, 0, site, 50, []Term{argument})
			solution := session.Solve()
			if _, ok := solution.Instantiation(site); ok || len(solution.manifest.instantiations) != 0 {
				t.Fatal("inactive guarded instantiation escaped into the solution")
			}
			if selected, ok := solution.Selection(choiceID); test.selected != ok || (ok && selected != 1) {
				t.Fatalf("selection=(%d,%v)", selected, ok)
			}
			root := session.find(argument.id)
			if root == 0 || session.cells[root-1].known != 0 {
				t.Fatalf("inactive argument defaulted: root=%d cell=%+v", root, session.cells[root-1])
			}
			if hasDiagnostic(diagnostics, CodeUnresolved) {
				t.Fatalf("inactive argument emitted T0510: %+v", diagnostics.Items())
			}
			if test.name == "inactive" {
				if diagnostics.HasErrors() {
					t.Fatalf("inactive branch emitted diagnostics: %+v", diagnostics.Items())
				}
			} else if test.name == "ambiguous" && (diagnostics.Len() != 1 || !hasDiagnostic(diagnostics, CodeAmbiguous)) {
				t.Fatalf("choice recovery leaked branch diagnostics: %+v", diagnostics.Items())
			}
		})
	}
}

func TestGuardedInstantiationValidationDuplicatesAndLimitsAreAtomic(t *testing.T) {
	program, store := testProgram(t)
	makeChoice := func(s *Session) ChoiceRef {
		value := s.Variable(Origin{})
		_, choice := s.AddChoice(OneOf([]Alternative{
			{Label: "int", Constraints: []Constraint{Equal(value, s.Known(store.Builtins().Int), Origin{})}},
			{Label: "bool", Constraints: []Constraint{Equal(value, s.Known(store.Builtins().Bool), Origin{})}},
		}, Origin{}))
		return choice
	}

	t.Run("shared site namespace", func(t *testing.T) {
		for _, ordinaryFirst := range []bool{true, false} {
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			choice := makeChoice(session)
			site := symbol.SyntaxRef{Module: 1, Node: 4}
			if ordinaryFirst {
				session.PublishInstantiation(site, 50, []Term{session.Known(store.Builtins().Int)})
				session.PublishGuardedInstantiation(choice, 0, site, 50, []Term{session.Known(store.Builtins().Int)})
			} else {
				session.PublishGuardedInstantiation(choice, 0, site, 50, []Term{session.Known(store.Builtins().Int)})
				session.PublishInstantiation(site, 50, []Term{session.Known(store.Builtins().Int)})
			}
			session.Solve()
			if len(session.instantiations) != 1 || session.instantiationArguments != 1 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
				t.Fatalf("ordinaryFirst=%v publications=%d arguments=%d diagnostics=%+v", ordinaryFirst, len(session.instantiations), session.instantiationArguments, diagnostics.Items())
			}
		}
	})

	t.Run("malformed inputs", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		choice := makeChoice(session)
		other := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		foreignChoice := makeChoice(other)
		foreignTerm := other.Known(store.Builtins().Int)
		validTerm := session.Known(store.Builtins().Int)
		session.PublishInstantiation(symbol.SyntaxRef{Module: 1, Node: 14}, 50, []Term{validTerm})
		forgedChoice := choice
		forgedChoice.alternatives++
		cases := []struct {
			choice      ChoiceRef
			alternative uint32
			site        symbol.SyntaxRef
			generic     symbol.SymbolID
			arguments   []Term
		}{
			{foreignChoice, 0, symbol.SyntaxRef{Module: 1, Node: 5}, 50, []Term{validTerm}},
			{forgedChoice, 0, symbol.SyntaxRef{Module: 1, Node: 13}, 50, []Term{validTerm}},
			{choice, 2, symbol.SyntaxRef{Module: 1, Node: 6}, 50, []Term{validTerm}},
			{choice, 0, symbol.SyntaxRef{}, 50, []Term{validTerm}},
			{choice, 0, symbol.SyntaxRef{Module: 1, Node: 7}, 0, []Term{validTerm}},
			{choice, 0, symbol.SyntaxRef{Module: 1, Node: 8}, 50, []Term{foreignTerm}},
			{choice, 0, symbol.SyntaxRef{Module: 1, Node: 9}, 50, []Term{{}}},
			{choice, 0, symbol.SyntaxRef{Module: 1, Node: 14}, 50, []Term{validTerm}},
		}
		for _, test := range cases {
			before := len(session.instantiations)
			beforeArguments := session.instantiationArguments
			callerArguments := append([]Term(nil), test.arguments...)
			session.PublishGuardedInstantiation(test.choice, test.alternative, test.site, test.generic, test.arguments)
			if len(session.instantiations) != before || session.instantiationArguments != beforeArguments {
				t.Fatalf("invalid publication mutated allocation: %+v map=%d->%d arguments=%d->%d", test, before, len(session.instantiations), beforeArguments, session.instantiationArguments)
			}
			for index := range test.arguments {
				if test.arguments[index] != callerArguments[index] {
					t.Fatalf("invalid publication mutated caller arguments: %+v", test)
				}
			}
		}
	})

	t.Run("component limits", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxShapeComponents: 1})
		choice := makeChoice(session)
		term := session.Known(store.Builtins().Int)
		session.PublishGuardedInstantiation(choice, 0, symbol.SyntaxRef{Module: 1, Node: 10}, 50, []Term{term, term})
		if len(session.instantiations) != 0 || session.instantiationArguments != 0 {
			t.Fatal("oversized argument list partially published")
		}
		session.PublishGuardedInstantiation(choice, 0, symbol.SyntaxRef{Module: 1, Node: 11}, 50, []Term{term})
		session.PublishGuardedInstantiation(choice, 0, symbol.SyntaxRef{Module: 1, Node: 12}, 50, []Term{term})
		session.Solve()
		if len(session.instantiations) != 1 || session.instantiationArguments != 1 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("publications=%d arguments=%d diagnostics=%+v", len(session.instantiations), session.instantiationArguments, diagnostics.Items())
		}
	})

	t.Run("cumulative argument limit", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxShapeComponents: 3})
		choice := makeChoice(session)
		term := session.Known(store.Builtins().Int)
		session.PublishInstantiation(symbol.SyntaxRef{Module: 1, Node: 20}, 50, []Term{term})
		session.PublishGuardedInstantiation(choice, 0, symbol.SyntaxRef{Module: 1, Node: 21}, 50, []Term{term})
		session.PublishInstantiation(symbol.SyntaxRef{Module: 1, Node: 22}, 50, []Term{term})
		if len(session.instantiations) != 3 || session.instantiationArguments != 3 {
			t.Fatalf("pre-limit allocation publications=%d arguments=%d", len(session.instantiations), session.instantiationArguments)
		}
		callerArguments := []Term{term}
		session.PublishGuardedInstantiation(choice, 1, symbol.SyntaxRef{Module: 1, Node: 23}, 50, callerArguments)
		if len(session.instantiations) != 3 || session.instantiationArguments != 3 || callerArguments[0] != term {
			t.Fatalf("over-limit publication mutated state: publications=%d arguments=%d caller=%+v", len(session.instantiations), session.instantiationArguments, callerArguments)
		}
		session.Solve()
		if !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("diagnostics=%+v", diagnostics.Items())
		}
	})

	t.Run("zero argument publication count limit", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxShapeComponents: 8})
		choice := makeChoice(session)
		for index := range 8 {
			site := symbol.SyntaxRef{Module: 1, Node: syntax.NodeID(30 + index)}
			if index%2 == 0 {
				session.PublishInstantiation(site, 50, nil)
			} else {
				session.PublishGuardedInstantiation(choice, uint32(index%2), site, 50, nil)
			}
		}
		if len(session.instantiations) != 8 || session.instantiationArguments != 0 {
			t.Fatalf("pre-limit allocation publications=%d arguments=%d", len(session.instantiations), session.instantiationArguments)
		}
		session.PublishGuardedInstantiation(choice, 0, symbol.SyntaxRef{Module: 1, Node: 38}, 50, nil)
		if len(session.instantiations) != 8 || session.instantiationArguments != 0 {
			t.Fatalf("over-limit zero-argument publication mutated state: publications=%d arguments=%d", len(session.instantiations), session.instantiationArguments)
		}
		session.Solve()
		if !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("diagnostics=%+v", diagnostics.Items())
		}
	})
}

func TestGuardedInstantiationSolutionAndSemanticSnapshotManifestsAreActiveAndOrdered(t *testing.T) {
	program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
	if preparation.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", preparation.Items())
	}
	box := semanticSymbol(t, program, "Box", symbol.SymbolType)
	refs := semanticSyntaxRefs(t, program, 3)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	selector := session.Variable(Origin{})
	session.Add(Equal(selector, session.Known(program.inputs.Types.Builtins().Bool), Origin{}))
	_, choice := session.AddChoice(OneOf([]Alternative{
		{Label: "inactive", Constraints: []Constraint{Equal(selector, session.Known(program.inputs.Types.Builtins().Int), Origin{})}},
		{Label: "active", Constraints: []Constraint{Equal(selector, session.Known(program.inputs.Types.Builtins().Bool), Origin{})}},
	}, Origin{}))
	session.PublishGuardedInstantiation(choice, 1, refs[2], box, []Term{session.Known(program.inputs.Types.Builtins().Char)})
	session.PublishGuardedInstantiation(choice, 0, refs[0], box, []Term{session.IntegerLiteral([]byte("42"), Origin{})})
	session.PublishInstantiation(refs[1], box, []Term{session.Known(program.inputs.Types.Builtins().Int)})
	solution := session.Solve()
	if !solution.Successful() || len(solution.manifest.instantiations) != 2 || solution.manifest.instantiations[0] != refs[1] || solution.manifest.instantiations[1] != refs[2] {
		t.Fatalf("solution=%v manifest=%+v diagnostics=%+v", solution.Successful(), solution.manifest.instantiations, diagnostics.Items())
	}
	if _, ok := solution.Instantiation(refs[0]); ok {
		t.Fatal("inactive publication entered solution table")
	}
	snapshotDiagnostics := diagnostic.NewDiagnosticSet()
	snapshot, ok := Snapshot(program, solution, snapshotDiagnostics)
	if !ok || snapshot == nil || !snapshot.Matches(solution) || snapshotDiagnostics.HasErrors() {
		t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, snapshotDiagnostics.Items())
	}
}
