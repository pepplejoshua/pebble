package infer

import (
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
)

func TestSessionFatalNilInvalidAndAllocationFree(t *testing.T) {
	var nilSession *Session
	if !nilSession.Fatal() {
		t.Fatal("nil session is not fatal")
	}

	diagnostics := diagnostic.NewDiagnosticSet()
	invalid := NewSession(nil, diagnostics, Config{})
	if !invalid.Fatal() {
		t.Fatal("invalid session is not fatal")
	}
	if allocations := testing.AllocsPerRun(100, func() {
		if !invalid.Fatal() {
			panic("fatal state changed")
		}
	}); allocations != 0 {
		t.Fatalf("Fatal allocated %v times per read", allocations)
	}
}

func TestSessionFatalBuilderPathsAndRecoveryBarrier(t *testing.T) {
	program, _ := testProgram(t)

	tests := []struct {
		name string
		run  func(*Session)
	}{
		{
			name: "ordinary limit",
			run: func(session *Session) {
				session.Variable(Origin{})
				session.Variable(Origin{})
			},
		},
		{
			name: "invalid term",
			run: func(session *Session) {
				foreign := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Variable(Origin{})
				session.NegateLiteral(foreign, Origin{})
			},
		},
		{
			name: "direct builder validation",
			run: func(session *Session) {
				session.AddChoice(Constraint{})
			},
		},
	}

	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			config := Config{}
			if test.name == "ordinary limit" {
				config.MaxInferVariables = 1
			}
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, config)
			test.run(session)
			if !session.Fatal() {
				t.Fatal("T0512 path did not become fatal immediately")
			}
			before := captureSessionMutationState(session)
			beforePending := len(session.reporter.items)

			attemptSessionMutations(session, program)

			if after := captureSessionMutationState(session); after != before {
				t.Fatalf("post-fatal mutation changed session state\nbefore: %+v\n after: %+v", before, after)
			}
			if len(session.reporter.items) != beforePending {
				t.Fatal("post-fatal mutation emitted a cascading diagnostic")
			}
			solution := session.Solve()
			if !session.Fatal() || solution == nil || !solution.finalized || solution.Successful() {
				t.Fatalf("fatal state did not persist through Solve: solution=%+v", solution)
			}
			if !reflect.DeepEqual(solution.SymbolTypes(), solution.SymbolTypes()) || !reflect.DeepEqual(solution.SyntaxTypes(), solution.SyntaxTypes()) || !reflect.DeepEqual(solution.Slots(), solution.Slots()) {
				t.Fatal("immutable fatal recovery queries changed between reads")
			}
		})
	}
}

func TestSessionFatalResolveTypeAndOccurrenceFailures(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`type Alias = int; fn use(value Alias) Alias => value;`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}
	ref := occurrenceRef(t, program, "Alias")

	t.Run("ResolveType", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		result := session.ResolveType(ref, ^symbol.SymbolID(0))
		if result.State != TypeError || !session.Fatal() {
			t.Fatalf("result=%+v fatal=%v", result, session.Fatal())
		}
	})

	t.Run("type occurrence", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		subject := session.Known(program.inputs.Types.Builtins().Int)
		if id := session.Add(TypeOccurrence(ref, ^symbol.SymbolID(0), subject, Origin{})); id != 0 || !session.Fatal() {
			t.Fatalf("constraint=%d fatal=%v", id, session.Fatal())
		}
	})
}

func TestSessionDiagnosticBudgetOverflowIsFatalBeforeFlush(t *testing.T) {
	program, _ := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{MaxDiagnostics: 1})
	session.IntegerLiteral([]byte("invalid"), Origin{Role: "first"})
	if session.Fatal() {
		t.Fatal("one retained nonfatal diagnostic made the session fatal")
	}
	session.IntegerLiteral([]byte("also_invalid"), Origin{Role: "overflow"})
	if !session.Fatal() {
		t.Fatal("diagnostic overflow was not fatal before flush")
	}
	if diagnostics.Len() != 0 {
		t.Fatal("test no longer observes the pre-flush state")
	}
	if len(session.reporter.items) != 1 || session.reporter.items[0].Code != CodeResourceLimit {
		t.Fatalf("pending diagnostics=%+v", session.reporter.items)
	}
	_ = session.Solve()
	if items := diagnostics.Items(); len(items) != 1 || items[0].Code != CodeResourceLimit {
		t.Fatalf("flushed diagnostics=%+v", items)
	}
}

func TestSessionFatalDuringSpeculationAndNonfatalRecovery(t *testing.T) {
	program, _ := testProgram(t)

	t.Run("fatal speculation", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		session.speculative = true
		session.conflict(CodeInvalidType, "recoverable branch", Origin{})
		if session.Fatal() {
			t.Fatal("T0501 made speculation fatal")
		}
		session.conflict(CodeResourceLimit, "fatal branch", Origin{})
		if !session.Fatal() || session.speculativeConflict == nil || session.speculativeConflict.code != CodeResourceLimit {
			t.Fatalf("fatal=%v conflict=%+v", session.Fatal(), session.speculativeConflict)
		}
	})

	t.Run("nonfatal diagnostic", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		session.NegateLiteral(session.Variable(Origin{}), Origin{})
		if session.Fatal() {
			t.Fatal("T0501 recovery made the session fatal")
		}
	})
}

func TestSessionMutationAndRepeatedSolveAfterCompletedSolve(t *testing.T) {
	program, _ := testProgram(t)

	t.Run("repeated Solve", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		first := session.Solve()
		second := session.Solve()
		if !session.Fatal() || first == second || second.finalized {
			t.Fatalf("fatal=%v first=%+v second=%+v", session.Fatal(), first, second)
		}
		count := diagnostics.Len()
		third := session.Solve()
		if third.finalized || diagnostics.Len() != count {
			t.Fatal("later repeated Solve cascaded diagnostics")
		}
	})

	t.Run("mutation", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		first := session.Solve()
		if session.Fatal() || first == nil || !first.finalized {
			t.Fatalf("initial solve: fatal=%v solution=%+v", session.Fatal(), first)
		}
		before := captureSessionMutationState(session)
		session.Variable(Origin{})
		if !session.Fatal() {
			t.Fatal("mutation after Solve did not become fatal")
		}
		if after := captureSessionMutationState(session); after != before {
			t.Fatalf("mutation after Solve changed session state\nbefore: %+v\n after: %+v", before, after)
		}
		count := diagnostics.Len()
		second := session.Solve()
		third := session.Solve()
		if second == first || third == first || second.finalized || third.finalized {
			t.Fatal("repeated Solve did not return immutable rejected recovery")
		}
		if diagnostics.Len() != count {
			t.Fatal("repeated Solve cascaded diagnostics after fatal state")
		}
	})
}

type sessionMutationState struct {
	cells, constraints, origins, symbolRoots, syntaxRoots, instantiations int
	methodStates, methodSites, resolveMemo, typeOccurrenceMemo            int
	valueOccurrenceMemo, requirements, selections, slots, publications    int
	shapeComponents, choiceCount                                          uint32
	unificationSteps, decompositionSteps, totalRequeues, choiceStates     uint64
	constraintCount, instantiationArguments                               uint64
}

func captureSessionMutationState(s *Session) sessionMutationState {
	return sessionMutationState{
		cells: len(s.cells), constraints: len(s.constraints), origins: len(s.origins),
		symbolRoots: len(s.symbolRoots), syntaxRoots: len(s.syntaxRoots), instantiations: len(s.instantiations),
		methodStates: len(s.methodStates), methodSites: len(s.methodSites), resolveMemo: len(s.resolveMemo),
		typeOccurrenceMemo: len(s.typeOccurrenceMemo), valueOccurrenceMemo: len(s.valueOccurrenceMemo),
		requirements: len(s.requirements), selections: len(s.selections), slots: len(s.slots), publications: len(s.slotPublications),
		shapeComponents: s.shapeComponents, choiceCount: s.choiceCount, constraintCount: s.constraintCount,
		unificationSteps: s.unificationSteps, decompositionSteps: s.decompositionSteps,
		totalRequeues: s.totalRequeues, choiceStates: s.choiceStates, instantiationArguments: s.instantiationArguments,
	}
}

func attemptSessionMutations(session *Session, program *Program) {
	recovery := session.Variable(Origin{})
	session.Known(program.inputs.Types.Builtins().Int)
	session.Error(Origin{})
	session.IntegerLiteral([]byte("1"), Origin{})
	session.FloatLiteral([]byte("1.0"), Origin{})
	session.NegateLiteral(recovery, Origin{})
	session.Add(Constraint{})
	session.AddChoice(Constraint{})
	session.PublishSlot(recovery)
	session.PublishSymbol(1, recovery)
	session.PublishSyntax(symbol.SyntaxRef{Module: 1, Node: 1}, recovery)
	session.PublishInstantiation(symbol.SyntaxRef{Module: 1, Node: 1}, 1, []Term{recovery})
	session.ResolveType(symbol.SyntaxRef{Module: 1, Node: 1}, 0)
}

func TestSessionFatalQueryDoesNotExposeOriginState(t *testing.T) {
	program, _ := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxInferVariables: 1})
	session.Variable(Origin{Span: source.NewSpan(1, 2, 3)})
	session.Variable(Origin{Span: source.NewSpan(4, 5, 6)})
	if !session.Fatal() {
		t.Fatal("fatal lifecycle state was not observable")
	}
}

func TestDiagnosticOverflowStopsUnresolvedFinalizationAtExactCell(t *testing.T) {
	program, _ := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
	first := session.Variable(Origin{Role: "first unresolved"})
	second := session.Variable(Origin{Role: "overflowing unresolved"})
	third := session.Variable(Origin{Role: "later unresolved"})

	session.finalizeUnresolved(nil)

	if !session.Fatal() {
		t.Fatal("unresolved diagnostic overflow was not fatal")
	}
	if !session.cells[first.id-1].error || session.cells[second.id-1].error || session.cells[third.id-1].error {
		t.Fatalf("finalization crossed overflow point: first=%+v second=%+v third=%+v", session.cells[first.id-1], session.cells[second.id-1], session.cells[third.id-1])
	}
	if session.unificationSteps != 0 || session.decompositionSteps != 0 || session.totalRequeues != 0 || session.choiceStates != 0 {
		t.Fatal("unresolved finalization changed solver counters")
	}
	assertDeterministicFatalSolutionQueries(t, session.Solve())
}

func TestDiagnosticOverflowStopsLiteralDefaultMutation(t *testing.T) {
	program, _ := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
	literal := session.IntegerLiteral([]byte("18446744073709551616"), Origin{Role: "oversized default"})
	session.reporter.error(CodeInvalidType, "retained nonfatal", Origin{Role: "retained"})
	before := session.cells[literal.id-1]

	session.defaultLiterals(nil)

	after := session.cells[literal.id-1]
	if !session.Fatal() || after.error || after.known != 0 || !reflect.DeepEqual(after, before) {
		t.Fatalf("literal mutated after overflow: fatal=%v\nbefore=%+v\n after=%+v", session.Fatal(), before, after)
	}
	if session.unificationSteps != 0 || session.decompositionSteps != 0 || session.totalRequeues != 0 || session.choiceStates != 0 {
		t.Fatal("literal defaulting changed unrelated solver counters")
	}
	assertDeterministicFatalSolutionQueries(t, session.Solve())
}

func TestDiagnosticOverflowDoesNotMemoizeFatalTypeResolution(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`type Alias = int; fn use(value Alias) Alias => value;`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}
	ref := occurrenceRef(t, program, "Alias")
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
	session.reporter.error(CodeInvalidType, "retained nonfatal", Origin{})

	result := session.ResolveType(ref, ^symbol.SymbolID(0))

	if result.State != TypeError || !session.Fatal() || len(session.resolveMemo) != 0 {
		t.Fatalf("result=%+v fatal=%v memo=%+v", result, session.Fatal(), session.resolveMemo)
	}
	assertDeterministicFatalSolutionQueries(t, session.Solve())
}

func TestDiagnosticOverflowDoesNotMemoizeFatalOccurrences(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`type Alias = int; fn use(value Alias) Alias => value;`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}
	ref := occurrenceRef(t, program, "Alias")

	t.Run("type", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
		session.reporter.error(CodeInvalidType, "retained nonfatal", Origin{})
		value := Constraint{kind: constraintTypeOccurrence, ref: symbol.SyntaxRef{Module: ref.Module, Node: ^syntax.NodeID(0)}, a: session.Known(program.inputs.Types.Builtins().Int)}

		if _, ok := session.applyTypeOccurrence(value); ok || !session.Fatal() || len(session.typeOccurrenceMemo) != 0 {
			t.Fatalf("fatal=%v memo=%+v", session.Fatal(), session.typeOccurrenceMemo)
		}
		assertDeterministicFatalSolutionQueries(t, session.Solve())
	})

	t.Run("value", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
		session.reporter.error(CodeInvalidType, "retained nonfatal", Origin{})
		value := Constraint{kind: constraintValueOccurrence, ref: symbol.SyntaxRef{Module: ref.Module, Node: ^syntax.NodeID(0)}}

		if session.applyValueOccurrence(value) || !session.Fatal() || len(session.valueOccurrenceMemo) != 0 {
			t.Fatalf("fatal=%v memo=%+v", session.Fatal(), session.valueOccurrenceMemo)
		}
		assertDeterministicFatalSolutionQueries(t, session.Solve())
	})
}

func TestDiagnosticOverflowStopsRootConflictMutation(t *testing.T) {
	program, _ := testProgram(t)

	for _, roots := range []int{1, 2} {
		t.Run(string(rune('0'+roots))+" roots", func(t *testing.T) {
			session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
			first := session.Variable(Origin{Role: "first root"})
			second := session.Variable(Origin{Role: "second root"})
			session.reporter.error(CodeInvalidType, "retained nonfatal", Origin{})
			beforeFirst := session.cells[first.id-1]
			beforeSecond := session.cells[second.id-1]

			if roots == 1 {
				session.markRootConflict(first.id, CodeUnification, "overflowing root conflict", Origin{})
			} else {
				session.markRootsConflict(first.id, second.id, CodeUnification, "overflowing roots conflict", Origin{})
			}

			if !session.Fatal() || !reflect.DeepEqual(session.cells[first.id-1], beforeFirst) || !reflect.DeepEqual(session.cells[second.id-1], beforeSecond) {
				t.Fatalf("root mutation crossed overflow: fatal=%v\nfirst=%+v\nsecond=%+v", session.Fatal(), session.cells[first.id-1], session.cells[second.id-1])
			}
			if session.unificationSteps != 0 || session.decompositionSteps != 0 || session.totalRequeues != 0 || session.choiceStates != 0 {
				t.Fatal("root conflict changed solver counters")
			}
			assertDeterministicFatalSolutionQueries(t, session.Solve())
		})
	}
}

func TestFatalSolveDoesNotClearSessionSelections(t *testing.T) {
	program, _ := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxDiagnostics: 1})
	choiceID, _ := session.AddChoice(testChoice(session, session.Variable(Origin{}), Origin{}))
	session.selections[choiceID] = 1
	session.conflict(CodeInvalidType, "retained nonfatal", Origin{})
	session.conflict(CodeUnification, "overflowing conflict", Origin{})

	solution := session.Solve()

	if selected, ok := session.selections[choiceID]; !ok || selected != 1 {
		t.Fatalf("fatal Solve mutated session selections: selection=(%d,%v)", selected, ok)
	}
	if _, ok := solution.Selection(choiceID); ok {
		t.Fatal("fatal solution published a guarded selection")
	}
	assertDeterministicFatalSolutionQueries(t, solution)
}

func assertDeterministicFatalSolutionQueries(t *testing.T, solution *Solution) {
	t.Helper()
	if solution == nil || !solution.finalized || solution.Successful() {
		t.Fatalf("fatal solution=%+v", solution)
	}
	if !reflect.DeepEqual(solution.SymbolTypes(), solution.SymbolTypes()) ||
		!reflect.DeepEqual(solution.SyntaxTypes(), solution.SyntaxTypes()) ||
		!reflect.DeepEqual(solution.Slots(), solution.Slots()) {
		t.Fatal("fatal solution queries are nondeterministic")
	}
}
