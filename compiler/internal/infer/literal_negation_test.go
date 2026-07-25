package infer

import (
	"math/big"
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func TestNegateLiteralConsumesExactIntegerInPlace(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	original := session.IntegerLiteral([]byte("9223372036854775808"), Origin{Role: "positive token"})
	before := captureSessionMutationState(session)
	negativeOrigin := Origin{Role: "unary minus"}
	negative := session.NegateLiteral(original, negativeOrigin)
	after := captureSessionMutationState(session)
	if negative != original || before != after || len(session.cells) != 1 {
		t.Fatalf("negation allocated or changed identity/counters: original=%+v negative=%+v before=%+v after=%+v", original, negative, before, after)
	}
	value := session.cells[original.id-1].literals[0]
	if value.integer.String() != "-9223372036854775808" || value.origin != negativeOrigin || session.cells[original.id-1].origin != negativeOrigin {
		t.Fatalf("negated value/origin = %s, %+v, %+v", value.integer, value.origin, session.cells[original.id-1].origin)
	}
	session.Add(LiteralFits(negative, session.Known(store.Builtins().I64), Origin{Role: "fit i64"}))
	session.PublishSymbol(1, negative)
	solution := session.Solve()
	got, ok := solution.SymbolType(1)
	if !solution.Successful() || !ok || got.Type != store.Builtins().I64 || diagnostics.HasErrors() {
		t.Fatalf("minimum i64 did not fit exactly: type=%+v diagnostics=%+v", got, diagnostics.Items())
	}
	for i := 0; i < 3; i++ {
		repeated, repeatedOK := solution.SymbolType(1)
		if !repeatedOK || repeated != got {
			t.Fatalf("query %d = %+v, %v; want %+v", i, repeated, repeatedOK, got)
		}
	}
}

func TestNegateLiteralRepeatedSignedBoundaries(t *testing.T) {
	for _, test := range []struct {
		name      string
		negations int
		success   bool
	}{
		{name: "triple negative fits", negations: 3, success: true},
		{name: "double negative fails", negations: 2, success: false},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, store := testProgram(t)
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			term := session.IntegerLiteral([]byte("128"), Origin{Role: "token"})
			for i := 0; i < test.negations; i++ {
				got := session.NegateLiteral(term, Origin{Role: "unary minus"})
				if got != term || len(session.cells) != 1 {
					t.Fatalf("negation %d changed term or cell count: got=%+v cells=%d", i, got, len(session.cells))
				}
			}
			session.Add(LiteralFits(term, session.Known(store.Builtins().I8), Origin{Role: "fit i8"}))
			session.PublishSymbol(1, term)
			solution := session.Solve()
			if solution.Successful() != test.success {
				t.Fatalf("successful=%v, want %v; diagnostics=%+v", solution.Successful(), test.success, diagnostics.Items())
			}
			if test.success && hasDiagnostic(diagnostics, CodeLiteral) {
				t.Fatalf("intermediate literal was fitted: %+v", diagnostics.Items())
			}
			if !test.success && !hasDiagnostic(diagnostics, CodeLiteral) {
				t.Fatalf("missing final positive overflow: %+v", diagnostics.Items())
			}
		})
	}
}

func TestNegateLiteralConsumesExactFloatInPlace(t *testing.T) {
	program, _ := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	term := session.FloatLiteral([]byte("1.25"), Origin{Role: "token"})
	before := captureSessionMutationState(session)
	got := session.NegateLiteral(term, Origin{Role: "minus"})
	if got != term || captureSessionMutationState(session) != before {
		t.Fatalf("float negation changed identity or counters: got=%+v", got)
	}
	want := big.NewRat(-5, 4)
	if value := session.cells[term.id-1].literals[0]; value.rational.Cmp(want) != 0 || value.kind != literalFloat {
		t.Fatalf("negated float = %+v, want %s", value, want)
	}
}

func TestNegateLiteralRejectsInvalidInputsAtomically(t *testing.T) {
	program, _ := testProgram(t)

	t.Run("foreign", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		foreign := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).IntegerLiteral([]byte("1"), Origin{})
		before := append([]ufCell(nil), session.cells...)
		if got := session.NegateLiteral(foreign, Origin{}); got.kind != termError || !session.Fatal() || !reflect.DeepEqual(session.cells, before) {
			t.Fatalf("foreign recovery mutated cells or was not fatal: got=%+v fatal=%v", got, session.Fatal())
		}
	})

	t.Run("nonliteral", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		variable := session.Variable(Origin{})
		before := append([]ufCell(nil), session.cells...)
		got := session.NegateLiteral(variable, Origin{})
		session.reporter.flush()
		if got.kind != termError || session.Fatal() || !reflect.DeepEqual(session.cells, before) || !hasDiagnostic(diagnostics, CodeInvalidType) {
			t.Fatalf("nonliteral recovery mismatch: got=%+v fatal=%v diagnostics=%+v", got, session.Fatal(), diagnostics.Items())
		}
	})

	for _, test := range []struct {
		name   string
		damage func(*Session, Term)
	}{
		{name: "missing payload", damage: func(s *Session, term Term) { s.cells[term.id-1].literals = nil }},
		{name: "wrong payload kind", damage: func(s *Session, term Term) {
			s.cells[term.id-1].literals[0] = literalValue{kind: literalFloat, rational: big.NewRat(1, 1)}
		}},
		{name: "unified component", damage: func(s *Session, term Term) {
			other := s.Variable(Origin{})
			s.cells[other.id-1].parent = term.id
			s.cells[term.id-1].rank = 1
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
			term := session.IntegerLiteral([]byte("7"), Origin{})
			test.damage(session, term)
			before := append([]ufCell(nil), session.cells...)
			if got := session.NegateLiteral(term, Origin{}); got.kind != termError || !session.Fatal() || !reflect.DeepEqual(session.cells, before) {
				t.Fatalf("malformed recovery mutated cells or was not fatal: got=%+v fatal=%v", got, session.Fatal())
			}
		})
	}

	t.Run("solved", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		term := session.IntegerLiteral([]byte("7"), Origin{})
		session.Solve()
		before := append([]ufCell(nil), session.cells...)
		if got := session.NegateLiteral(term, Origin{}); got.kind != termError || !session.Fatal() || !reflect.DeepEqual(session.cells, before) {
			t.Fatalf("solved recovery mutated cells or was not fatal: got=%+v fatal=%v", got, session.Fatal())
		}
	})

	t.Run("fatal", func(t *testing.T) {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{MaxInferVariables: 1})
		term := session.IntegerLiteral([]byte("7"), Origin{})
		session.Variable(Origin{})
		before := append([]ufCell(nil), session.cells...)
		if got := session.NegateLiteral(term, Origin{}); got.kind != termError || !session.Fatal() || !reflect.DeepEqual(session.cells, before) {
			t.Fatalf("fatal recovery mutated cells: got=%+v fatal=%v", got, session.Fatal())
		}
	})
}
