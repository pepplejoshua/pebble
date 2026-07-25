package infer

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestHasComponentKnownTupleAndShapeWithUnresolvedSibling(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	// Create a tuple type (i32, str)
	tupleType := mustType(t, store, types.TupleKey([]types.TypeID{b.I32, b.Str}))

	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	component0 := session.Variable(Origin{})
	component1 := session.Variable(Origin{})

	// Test tuple component access with known tuple type
	session.Add(HasComponent(session.Known(tupleType), 0, component0, Origin{}))
	session.Add(HasComponent(session.Known(tupleType), 1, component1, Origin{}))

	ids := []SlotID{
		session.PublishSlot(component0),
		session.PublishSlot(component1),
	}

	solution := session.Solve()
	if !solution.Successful() {
		t.Fatal("known tuple component access did not solve")
	}
	assertSlotType(t, solution, ids[0], b.I32)
	assertSlotType(t, solution, ids[1], b.Str)
}

// TestHasComponentShapeWithUnresolvedSibling proves the constraint resolves
// its own component from a fixed-arity shape without waiting on a sibling
// leaf that is not yet resolved when HasComponent examines the shape. The
// sibling is pinned down by a separate, independent Equal constraint so the
// whole session still reaches a successful solve; the property under test is
// that HasComponent's own resolution of ordinal 0 never inspects or requires
// progress on ordinal 1 to do so (its structural.go implementation only
// reads shape.children[ordinal], never any other index). This is the case
// TestHasComponentKnownTupleAndShapeWithUnresolvedSibling's name promises but
// does not actually exercise (it only tests a fully known tuple type), and
// TestHasComponentShapeWithFixedArity does not exercise either (every leaf
// there is already known up front).
func TestHasComponentShapeWithUnresolvedSibling(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	receiver := session.Variable(Origin{})
	sibling := session.Variable(Origin{}) // unresolved when HasComponent(0) runs
	component0 := session.Variable(Origin{})

	session.Add(ConstrainShape(receiver, TupleShape([]Shape{
		Leaf(session.Known(b.I32)),
		Leaf(sibling),
	}), Origin{}))
	session.Add(HasComponent(receiver, 0, component0, Origin{}))
	// Independent equation, added after: the sibling's own resolution must
	// not be a precondition for ordinal 0 to resolve above.
	session.Add(Equal(sibling, session.Known(b.Str), Origin{}))

	component0Slot := session.PublishSlot(component0)

	solution := session.Solve()
	if !solution.Successful() {
		t.Fatalf("component 0 should resolve without waiting on the sibling: %v", solution.Successful())
	}
	assertSlotType(t, solution, component0Slot, b.I32)
}

func TestHasComponentShapeWithFixedArity(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	shapeSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	receiver := shapeSession.Variable(Origin{})
	component0 := shapeSession.Variable(Origin{})

	// Shape constraint with fixed arity - access component0, sibling is known but unused
	shapeSession.Add(HasComponent(receiver, 0, component0, Origin{}))
	shapeSession.Add(ConstrainShape(receiver, TupleShape([]Shape{
		Leaf(shapeSession.Known(b.I32)),
		Leaf(shapeSession.Known(b.Str)),
	}), Origin{}))

	component0Slot := shapeSession.PublishSlot(component0)

	shapeSolution := shapeSession.Solve()
	if !shapeSolution.Successful() {
		t.Fatal("shape-based tuple component access did not solve")
	}
	assertSlotType(t, shapeSolution, component0Slot, b.I32)
}

func TestHasComponentOrderIndependenceShapeFirst(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	shapeSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	receiver := shapeSession.Variable(Origin{})
	component0 := shapeSession.Variable(Origin{})
	component1 := shapeSession.Variable(Origin{})

	// Add shape constraint BEFORE component constraint
	shapeSession.Add(ConstrainShape(receiver, TupleShape([]Shape{
		Leaf(shapeSession.Known(b.I32)),
		Leaf(shapeSession.Known(b.Str)),
	}), Origin{}))
	shapeSession.Add(HasComponent(receiver, 0, component0, Origin{}))
	shapeSession.Add(HasComponent(receiver, 1, component1, Origin{}))

	component0Slot := shapeSession.PublishSlot(component0)
	component1Slot := shapeSession.PublishSlot(component1)

	shapeSolution := shapeSession.Solve()
	if !shapeSolution.Successful() {
		t.Fatal("shape-first equation order did not solve")
	}
	assertSlotType(t, shapeSolution, component0Slot, b.I32)
	assertSlotType(t, shapeSolution, component1Slot, b.Str)
}

func TestHasComponentOutOfRangeOrdinal(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	// Create a tuple type with 2 elements
	tupleType := mustType(t, store, types.TupleKey([]types.TypeID{b.I32, b.Str}))

	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	result := session.Variable(Origin{})

	// Try to access element at index 2 (out of range for 2-element tuple)
	session.Add(HasComponent(session.Known(tupleType), 2, result, Origin{}))
	session.PublishSlot(result)

	session.Solve()
	if !hasDiagnostic(diagnostics, CodeCapability) || hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("out-of-range diagnostics=%+v", diagnostics.Items())
	}
}

func TestHasComponentNonTupleReceiver(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	result := session.Variable(Origin{})

	// Try to access component on a non-tuple type
	session.Add(HasComponent(session.Known(b.I32), 0, result, Origin{}))
	session.PublishSlot(result)

	session.Solve()
	if !hasDiagnostic(diagnostics, CodeCapability) || hasDiagnostic(diagnostics, CodeUnresolved) {
		t.Fatalf("non-tuple diagnostics=%+v", diagnostics.Items())
	}
}

func TestHasComponentRigidTypeParameterRecovery(t *testing.T) {
	program, store := testProgram(t)

	rigid := mustType(t, store, types.TypeParameterKey(symbol.SymbolID(902)))

	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	result := session.Variable(Origin{})

	session.Add(HasComponent(session.Known(rigid), 0, result, Origin{}))
	slot := session.PublishSlot(result)

	solution := session.Solve()
	if diagnostics.HasErrors() {
		t.Fatalf("rigid diagnostics=%+v", diagnostics.Items())
	}
	assertSlotError(t, solution, slot)
}

func TestHasComponentErrorReceiver(t *testing.T) {
	program, _ := testProgram(t)

	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})

	session.Add(HasComponent(session.Error(Origin{}), 0, session.Error(Origin{}), Origin{}))

	solution := session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("error suppression solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
	}
}
