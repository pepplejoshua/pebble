package infer

import (
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestSolvedSlotsAndInactiveGuardedPublication(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	selector := session.Variable(Origin{Role: "selector"})
	session.Add(Equal(selector, session.Known(store.Builtins().Bool), Origin{}))
	inactive := session.IntegerLiteral([]byte("42"), Origin{Role: "inactive literal"})
	active := session.Variable(Origin{Role: "active result"})
	choiceID, choice := session.AddChoice(OneOf([]Alternative{
		{Label: "integer", Constraints: []Constraint{Equal(selector, session.Known(store.Builtins().Int), Origin{})}},
		{Label: "boolean", Constraints: []Constraint{Equal(active, session.Known(store.Builtins().Char), Origin{})}},
	}, Origin{Role: "choice"}))
	ordinaryID := session.PublishSlot(session.Known(store.Builtins().Uint))
	inactiveID := session.PublishGuardedSlot(choice, 0, inactive)
	activeID := session.PublishGuardedSlot(choice, 1, active)
	before := store.Len()
	solution := session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() || store.Len() != before {
		t.Fatalf("solution=%v diagnostics=%+v store=%d->%d", solution.Successful(), diagnostics.Items(), before, store.Len())
	}
	if selected, ok := solution.Selection(choiceID); !ok || selected != 1 {
		t.Fatalf("selection=(%d,%v)", selected, ok)
	}
	assertSlotType(t, solution, ordinaryID, store.Builtins().Uint)
	assertSlotType(t, solution, activeID, store.Builtins().Char)
	if _, ok := solution.Slot(inactiveID); ok {
		t.Fatal("inactive guarded slot was published")
	}
	slots := solution.Slots()
	if len(slots) != 2 || slots[0].Slot != ordinaryID || slots[1].Slot != activeID {
		t.Fatalf("ordered slots=%+v", slots)
	}
	slots[0].Result = TypeResult{State: TypeError}
	assertSlotType(t, solution, ordinaryID, store.Builtins().Uint)
}

func TestSlotPublicationValidationLimitAndSnapshotOwnership(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{MaxSolvedSlots: 1})
	term := session.Known(store.Builtins().Int)
	first := session.PublishSlot(term)
	if first == (SlotID{}) {
		t.Fatal("first slot was rejected")
	}
	if duplicate := session.PublishSlot(term); duplicate != (SlotID{}) {
		t.Fatal("duplicate slot was accepted")
	}
	if overflow := session.PublishSlot(session.Known(store.Builtins().Bool)); overflow != (SlotID{}) {
		t.Fatal("slot beyond configured limit was accepted")
	}
	solution := session.Solve()
	if solution.Successful() || !hasDiagnostic(diagnostics, CodeResourceLimit) || len(solution.Slots()) != 1 {
		t.Fatalf("solution=%v slots=%+v diagnostics=%+v", solution.Successful(), solution.Slots(), diagnostics.Items())
	}

	otherDiagnostics := diagnostic.NewDiagnosticSet()
	other := NewSession(program, otherDiagnostics, Config{})
	if got := other.PublishSlot(term); got != (SlotID{}) {
		t.Fatal("foreign term slot was accepted")
	}
	if _, ok := other.Solve().Slot(first); ok {
		t.Fatal("slot ID crossed solution snapshots")
	}
}

func TestAddChoiceAndGuardValidationAreAtomic(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	v := session.Variable(Origin{})
	if id, ref := session.AddChoice(Equal(v, session.Known(store.Builtins().Int), Origin{})); id != 0 || ref != (ChoiceRef{}) {
		t.Fatal("AddChoice accepted a non-OneOf constraint")
	}
	if len(session.constraints) != 0 {
		t.Fatal("failed AddChoice allocated a constraint")
	}

	other := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	otherV := other.Variable(Origin{})
	_, foreign := other.AddChoice(OneOf([]Alternative{
		{Label: "int", Constraints: []Constraint{Equal(otherV, other.Known(store.Builtins().Int), Origin{})}},
		{Label: "bool", Constraints: []Constraint{Equal(otherV, other.Known(store.Builtins().Bool), Origin{})}},
	}, Origin{}))
	before := len(session.slots)
	if got := session.PublishGuardedSlot(foreign, 0, v); got != (SlotID{}) || len(session.slots) != before {
		t.Fatal("foreign guarded publication was not atomic")
	}
	session.Solve()
	if !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestCallableKnownShapeVariadicAndCopiedArguments(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	knownFunction, err := store.Intern(types.FunctionKey(types.C, []types.TypeID{b.I32}, b.Bool, false))
	if err != nil {
		t.Fatal(err)
	}
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	source := session.Known(b.Str)
	destination := session.Variable(Origin{Role: "destination"})
	result := session.Variable(Origin{Role: "result"})
	arguments := []CallableArgument{{Source: source, Destination: destination}}
	constraint := Callable(session.Known(knownFunction), arguments, result, Origin{})
	arguments[0] = CallableArgument{}
	session.Add(constraint)
	destinationSlot := session.PublishSlot(destination)
	resultSlot := session.PublishSlot(result)
	solution := session.Solve()
	if !solution.Successful() {
		t.Fatal("known callable did not solve")
	}
	assertSlotType(t, solution, destinationSlot, b.I32)
	assertSlotType(t, solution, resultSlot, b.Bool)
	key, _ := store.Key(knownFunction)
	convention, _, _, _, _ := key.Function()
	if convention != types.C {
		t.Fatalf("calling convention changed to %d", convention)
	}

	shapeSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	callee := shapeSession.Variable(Origin{})
	fixedDestination := shapeSession.Variable(Origin{})
	tailSource := shapeSession.Known(b.Char)
	tailDestination := shapeSession.Variable(Origin{})
	shapeResult := shapeSession.Variable(Origin{})
	shapeSession.Add(Callable(callee, []CallableArgument{
		{Source: shapeSession.Known(b.Str), Destination: fixedDestination},
		{Source: tailSource, Destination: tailDestination},
	}, shapeResult, Origin{}))
	shapeSession.Add(ConstrainShape(callee, FunctionShape(types.Pebble, []Shape{Leaf(shapeSession.Known(b.Uint))}, Leaf(shapeSession.Known(b.Void)), true), Origin{}))
	fixedSlot := shapeSession.PublishSlot(fixedDestination)
	tailSlot := shapeSession.PublishSlot(tailDestination)
	shapeResultSlot := shapeSession.PublishSlot(shapeResult)
	shapeSolution := shapeSession.Solve()
	if !shapeSolution.Successful() {
		t.Fatal("delayed callable shape did not solve")
	}
	assertSlotType(t, shapeSolution, fixedSlot, b.Uint)
	assertSlotType(t, shapeSolution, tailSlot, b.Char)
	assertSlotType(t, shapeSolution, shapeResultSlot, b.Void)
}

func TestCallableFailuresRigidRecoveryAndRequeueLimit(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	for _, test := range []struct {
		name   string
		callee types.TypeID
		args   int
	}{
		{name: "non-function", callee: b.Bool},
		{name: "arity", callee: mustType(t, store, types.FunctionKey(types.Pebble, []types.TypeID{b.Int}, b.Void, false)), args: 2},
	} {
		t.Run(test.name, func(t *testing.T) {
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			arguments := make([]CallableArgument, test.args)
			for i := range arguments {
				arguments[i] = CallableArgument{Source: session.Known(b.Int), Destination: session.Variable(Origin{})}
			}
			result := session.Variable(Origin{})
			session.Add(Callable(session.Known(test.callee), arguments, result, Origin{}))
			session.PublishSlot(result)
			session.Solve()
			if !hasDiagnostic(diagnostics, CodeCapability) || hasDiagnostic(diagnostics, CodeUnresolved) {
				t.Fatalf("diagnostics=%+v", diagnostics.Items())
			}
		})
	}

	rigid := mustType(t, store, types.TypeParameterKey(symbol.SymbolID(900)))
	rigidDiagnostics := diagnostic.NewDiagnosticSet()
	rigidSession := NewSession(program, rigidDiagnostics, Config{})
	destination := rigidSession.Variable(Origin{})
	result := rigidSession.Variable(Origin{})
	rigidSession.Add(Callable(rigidSession.Known(rigid), []CallableArgument{{Source: rigidSession.Known(b.Int), Destination: destination}}, result, Origin{}))
	destinationSlot := rigidSession.PublishSlot(destination)
	resultSlot := rigidSession.PublishSlot(result)
	rigidSolution := rigidSession.Solve()
	if rigidSolution.Successful() || rigidDiagnostics.HasErrors() {
		t.Fatalf("rigid solution=%v diagnostics=%+v", rigidSolution.Successful(), rigidDiagnostics.Items())
	}
	assertSlotError(t, rigidSolution, destinationSlot)
	assertSlotError(t, rigidSolution, resultSlot)

	limitDiagnostics := diagnostic.NewDiagnosticSet()
	limitSession := NewSession(program, limitDiagnostics, Config{MaxConstraintRequeues: 1})
	unresolved := limitSession.Variable(Origin{})
	limitSession.Add(Callable(unresolved, nil, limitSession.Variable(Origin{}), Origin{}))
	other := limitSession.Variable(Origin{})
	limitSession.Add(Equal(other, limitSession.Known(b.Bool), Origin{}))
	limitSession.Solve()
	if !hasDiagnostic(limitDiagnostics, CodeResourceLimit) {
		t.Fatalf("requeue diagnostics=%+v", limitDiagnostics.Items())
	}
}

func TestIndexableAndSliceableKnownAndShapeRelations(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	array := mustType(t, store, types.ArrayKey(4, b.I16))
	slice := mustType(t, store, types.SliceKey(b.U32))

	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	arrayElement := session.Variable(Origin{})
	sliceElement := session.Variable(Origin{})
	stringElement := session.Variable(Origin{})
	arraySlice := session.Variable(Origin{})
	stringSlice := session.Variable(Origin{})
	session.Add(Indexable(session.Known(array), arrayElement, Origin{}))
	session.Add(Indexable(session.Known(slice), sliceElement, Origin{}))
	session.Add(Indexable(session.Known(b.Str), stringElement, Origin{}))
	session.Add(Sliceable(session.Known(array), arraySlice, Origin{}))
	session.Add(Sliceable(session.Known(b.Str), stringSlice, Origin{}))
	ids := []SlotID{
		session.PublishSlot(arrayElement), session.PublishSlot(sliceElement), session.PublishSlot(stringElement),
		session.PublishSlot(arraySlice), session.PublishSlot(stringSlice),
	}
	solution := session.Solve()
	if !solution.Successful() {
		t.Fatal("known structural relations did not solve")
	}
	assertSlotType(t, solution, ids[0], b.I16)
	assertSlotType(t, solution, ids[1], b.U32)
	assertSlotType(t, solution, ids[2], b.Char)
	assertSlotType(t, solution, ids[3], mustType(t, store, types.SliceKey(b.I16)))
	assertSlotType(t, solution, ids[4], b.Str)

	shapeSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	receiver := shapeSession.Variable(Origin{})
	indexed := shapeSession.Variable(Origin{})
	sliced := shapeSession.Variable(Origin{})
	shapeSession.Add(Indexable(receiver, indexed, Origin{}))
	shapeSession.Add(Sliceable(receiver, sliced, Origin{}))
	shapeSession.Add(ConstrainShape(receiver, ArrayShape(3, Leaf(shapeSession.Known(b.Uint))), Origin{}))
	indexedSlot := shapeSession.PublishSlot(indexed)
	slicedSlot := shapeSession.PublishSlot(sliced)
	shapeSolution := shapeSession.Solve()
	if !shapeSolution.Successful() {
		t.Fatal("shape structural relations did not solve")
	}
	assertSlotType(t, shapeSolution, indexedSlot, b.Uint)
	assertSlotType(t, shapeSolution, slicedSlot, mustType(t, store, types.SliceKey(b.Uint)))
}

func TestStructuralFieldRelations(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	slice := mustType(t, store, types.SliceKey(b.I32))
	data := mustType(t, store, types.PointerKey(b.I32))
	array := mustType(t, store, types.ArrayKey(5, b.I32))
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	lenValue := session.Variable(Origin{})
	dataValue := session.Variable(Origin{})
	arrayLen := session.Variable(Origin{})
	session.Add(StructuralField(session.Known(slice), "len", lenValue, Origin{}))
	session.Add(StructuralField(session.Known(slice), "data", dataValue, Origin{}))
	session.Add(StructuralField(session.Known(array), "len", arrayLen, Origin{}))
	lenSlot := session.PublishSlot(lenValue)
	dataSlot := session.PublishSlot(dataValue)
	arraySlot := session.PublishSlot(arrayLen)
	solution := session.Solve()
	if !solution.Successful() {
		t.Fatal("structural fields did not solve")
	}
	assertSlotType(t, solution, lenSlot, b.Uint)
	assertSlotType(t, solution, dataSlot, data)
	assertSlotType(t, solution, arraySlot, b.Uint)
}

func TestStructuralFieldRelationsThroughPointer(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	slice := mustType(t, store, types.SliceKey(b.I32))
	pointerSlice := mustType(t, store, types.PointerKey(slice))
	data := mustType(t, store, types.PointerKey(b.I32))
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	length := session.Variable(Origin{})
	dataValue := session.Variable(Origin{})
	session.Add(StructuralField(session.Known(pointerSlice), "len", length, Origin{}))
	session.Add(StructuralField(session.Known(pointerSlice), "data", dataValue, Origin{}))
	lengthSlot := session.PublishSlot(length)
	dataSlot := session.PublishSlot(dataValue)
	solution := session.Solve()
	if !solution.Successful() {
		t.Fatalf("pointer structural fields did not solve")
	}
	assertSlotType(t, solution, lengthSlot, b.Uint)
	assertSlotType(t, solution, dataSlot, data)
}

func TestStructuralFieldRelationsThroughDeferredPointerPointee(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	slice := mustType(t, store, types.SliceKey(b.I32))
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	pointee := session.Variable(Origin{})
	receiver := session.Variable(Origin{})
	length := session.Variable(Origin{})
	session.Add(Equal(pointee, session.Known(slice), Origin{}))
	session.Add(ConstrainShape(receiver, PointerShape(Leaf(pointee)), Origin{}))
	session.Add(StructuralField(receiver, "len", length, Origin{}))
	lengthSlot := session.PublishSlot(length)
	if solution := session.Solve(); !solution.Successful() {
		t.Fatal("deferred pointer pointee structural field did not solve")
	} else {
		assertSlotType(t, solution, lengthSlot, b.Uint)
	}
}

func TestOptionalHasValueStructuralFieldRelations(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	optional := mustType(t, store, types.OptionalKey(b.I32))
	pointerOptional := mustType(t, store, types.PointerKey(optional))
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	hasValue := session.Variable(Origin{})
	pointerHasValue := session.Variable(Origin{})
	session.Add(StructuralField(session.Known(optional), "has_value", hasValue, Origin{}))
	session.Add(StructuralField(session.Known(pointerOptional), "has_value", pointerHasValue, Origin{}))
	hasValueSlot := session.PublishSlot(hasValue)
	pointerHasValueSlot := session.PublishSlot(pointerHasValue)
	solution := session.Solve()
	if !solution.Successful() {
		t.Fatal("optional structural field did not solve")
	}
	assertSlotType(t, solution, hasValueSlot, b.Bool)
	assertSlotType(t, solution, pointerHasValueSlot, b.Bool)
}

func TestOptionalUnknownStructuralFieldRejected(t *testing.T) {
	program, store := testProgram(t)
	optional := mustType(t, store, types.OptionalKey(store.Builtins().I32))
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	field := session.Variable(Origin{})
	session.Add(StructuralField(session.Known(optional), "foo", field, Origin{}))
	session.PublishSlot(field)
	session.Solve()
	if !hasDiagnostic(diagnostics, CodeCapability) {
		t.Fatalf("unknown optional field diagnostics=%+v", diagnostics.Items())
	}
}

func TestStructuralFailuresCrossSessionAndRigidRecovery(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	for _, build := range []func(Term, Term, Origin) Constraint{Indexable, Sliceable} {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		result := session.Variable(Origin{})
		session.Add(build(session.Known(b.Bool), result, Origin{}))
		session.PublishSlot(result)
		session.Solve()
		if !hasDiagnostic(diagnostics, CodeCapability) || hasDiagnostic(diagnostics, CodeUnresolved) {
			t.Fatalf("failure diagnostics=%+v", diagnostics.Items())
		}
	}

	rigid := mustType(t, store, types.TypeParameterKey(symbol.SymbolID(901)))
	for _, build := range []func(Term, Term, Origin) Constraint{Indexable, Sliceable} {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		result := session.Variable(Origin{})
		session.Add(build(session.Known(rigid), result, Origin{}))
		slot := session.PublishSlot(result)
		solution := session.Solve()
		if diagnostics.HasErrors() {
			t.Fatalf("rigid diagnostics=%+v", diagnostics.Items())
		}
		assertSlotError(t, solution, slot)
	}

	first := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	secondDiagnostics := diagnostic.NewDiagnosticSet()
	second := NewSession(program, secondDiagnostics, Config{})
	second.Add(Indexable(first.Known(b.Str), second.Variable(Origin{}), Origin{}))
	second.Solve()
	if !hasDiagnostic(secondDiagnostics, CodeResourceLimit) {
		t.Fatalf("cross-session diagnostics=%+v", secondDiagnostics.Items())
	}
}

func TestDelayedStructuralConstraintsUseUnresolvedRecoveryAndSuppressError(t *testing.T) {
	program, _ := testProgram(t)
	for _, build := range []func(*Session) Constraint{
		func(session *Session) Constraint {
			return Callable(session.Variable(Origin{Role: "callee"}), nil, session.Variable(Origin{Role: "result"}), Origin{})
		},
		func(session *Session) Constraint {
			return Indexable(session.Variable(Origin{Role: "receiver"}), session.Variable(Origin{Role: "result"}), Origin{})
		},
		func(session *Session) Constraint {
			return Sliceable(session.Variable(Origin{Role: "receiver"}), session.Variable(Origin{Role: "result"}), Origin{})
		},
	} {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		session.Add(build(session))
		session.Solve()
		if !hasDiagnostic(diagnostics, CodeUnresolved) || hasDiagnostic(diagnostics, CodeCapability) {
			t.Fatalf("unresolved diagnostics=%+v", diagnostics.Items())
		}
	}

	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	session.Add(Callable(session.Error(Origin{}), nil, session.Error(Origin{}), Origin{}))
	session.Add(Indexable(session.Error(Origin{}), session.Error(Origin{}), Origin{}))
	session.Add(Sliceable(session.Error(Origin{}), session.Error(Origin{}), Origin{}))
	solution := session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("error suppression solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
	}
}

func TestCallableErrorPropagationPreservesUnaffectedOutputs(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	fixed := mustType(t, store, types.FunctionKey(types.C, []types.TypeID{b.I32, b.U64}, b.Bool, false))

	t.Run("mixed fixed arguments", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		firstDestination := session.Variable(Origin{Role: "first destination"})
		result := session.Variable(Origin{Role: "result"})
		session.Add(Callable(session.Known(fixed), []CallableArgument{
			{Source: session.Error(Origin{}), Destination: firstDestination},
			{Source: session.Known(b.Char), Destination: session.Error(Origin{})},
		}, result, Origin{}))
		firstSlot := session.PublishSlot(firstDestination)
		errorSlot := session.PublishSlot(session.Error(Origin{}))
		resultSlot := session.PublishSlot(result)
		solution := session.Solve()
		if solution.Successful() || diagnostics.HasErrors() {
			t.Fatalf("solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
		}
		assertSlotType(t, solution, firstSlot, b.I32)
		assertSlotError(t, solution, errorSlot)
		assertSlotType(t, solution, resultSlot, b.Bool)
	})

	t.Run("error result", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		first := session.Variable(Origin{})
		second := session.Variable(Origin{})
		errorResult := session.Error(Origin{})
		session.Add(Callable(session.Known(fixed), []CallableArgument{
			{Source: session.Known(b.Str), Destination: first},
			{Source: session.Known(b.Str), Destination: second},
		}, errorResult, Origin{}))
		firstSlot := session.PublishSlot(first)
		secondSlot := session.PublishSlot(second)
		resultSlot := session.PublishSlot(errorResult)
		solution := session.Solve()
		if solution.Successful() || diagnostics.HasErrors() {
			t.Fatalf("solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
		}
		assertSlotType(t, solution, firstSlot, b.I32)
		assertSlotType(t, solution, secondSlot, b.U64)
		assertSlotError(t, solution, resultSlot)
	})
}

func TestCallableErrorCalleeAndVariadicSourceRecoverDependentOutputs(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()

	t.Run("error callee", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		first := session.Variable(Origin{})
		second := session.Variable(Origin{})
		result := session.Variable(Origin{})
		session.Add(Callable(session.Error(Origin{}), []CallableArgument{
			{Source: session.Known(b.Int), Destination: first},
			{Source: session.Error(Origin{}), Destination: second},
		}, result, Origin{}))
		slots := []SlotID{session.PublishSlot(first), session.PublishSlot(second), session.PublishSlot(result)}
		solution := session.Solve()
		if solution.Successful() || diagnostics.HasErrors() {
			t.Fatalf("solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
		}
		for _, slot := range slots {
			assertSlotError(t, solution, slot)
		}
	})

	t.Run("variadic error source", func(t *testing.T) {
		variadic := mustType(t, store, types.FunctionKey(types.Pebble, []types.TypeID{b.I16}, b.Void, true))
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		fixedDestination := session.Variable(Origin{})
		errorTailDestination := session.Variable(Origin{})
		goodTailDestination := session.Variable(Origin{})
		result := session.Variable(Origin{})
		session.Add(Callable(session.Known(variadic), []CallableArgument{
			{Source: session.Error(Origin{}), Destination: fixedDestination},
			{Source: session.Error(Origin{}), Destination: errorTailDestination},
			{Source: session.Known(b.Char), Destination: goodTailDestination},
		}, result, Origin{}))
		fixedSlot := session.PublishSlot(fixedDestination)
		errorTailSlot := session.PublishSlot(errorTailDestination)
		goodTailSlot := session.PublishSlot(goodTailDestination)
		resultSlot := session.PublishSlot(result)
		solution := session.Solve()
		if solution.Successful() || diagnostics.HasErrors() {
			t.Fatalf("solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
		}
		assertSlotType(t, solution, fixedSlot, b.I16)
		assertSlotError(t, solution, errorTailSlot)
		assertSlotType(t, solution, goodTailSlot, b.Char)
		assertSlotType(t, solution, resultSlot, b.Void)
	})
}

func TestErrorReceiverRecoversIndexAndSliceResults(t *testing.T) {
	program, _ := testProgram(t)
	for name, build := range map[string]func(Term, Term, Origin) Constraint{
		"indexable": Indexable,
		"sliceable": Sliceable,
	} {
		t.Run(name, func(t *testing.T) {
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			result := session.Variable(Origin{Role: "dependent result"})
			session.Add(build(session.Error(Origin{}), result, Origin{}))
			slot := session.PublishSlot(result)
			solution := session.Solve()
			if solution.Successful() || diagnostics.HasErrors() || hasDiagnostic(diagnostics, CodeCapability) || hasDiagnostic(diagnostics, CodeUnresolved) {
				t.Fatalf("solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
			}
			assertSlotError(t, solution, slot)
		})
	}
}

func TestStructuralChoiceRollbackRestoresLosingAlternative(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	array := mustType(t, store, types.ArrayKey(2, b.I16))
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	selector := session.Variable(Origin{})
	result := session.Variable(Origin{})
	session.Add(Equal(selector, session.Known(b.Bool), Origin{}))
	choiceID, _ := session.AddChoice(OneOf([]Alternative{
		{Label: "losing array", Constraints: []Constraint{
			Indexable(session.Known(array), result, Origin{}),
			Equal(selector, session.Known(b.Int), Origin{}),
		}},
		{Label: "string", Constraints: []Constraint{Indexable(session.Known(b.Str), result, Origin{})}},
	}, Origin{}))
	slot := session.PublishSlot(result)
	solution := session.Solve()
	selected, ok := solution.Selection(choiceID)
	if !solution.Successful() || diagnostics.HasErrors() || !ok || selected != 1 {
		t.Fatalf("selection=(%d,%v) solution=%v diagnostics=%+v", selected, ok, solution.Successful(), diagnostics.Items())
	}
	assertSlotType(t, solution, slot, b.Char)
}

func TestNewStructuralRelationsAreEquationOrderIndependent(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	array := mustType(t, store, types.ArrayKey(2, b.I64))
	run := func(reverse bool) types.TypeKey {
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		receiver := session.Variable(Origin{})
		result := session.Variable(Origin{})
		facts := []Constraint{Sliceable(receiver, result, Origin{}), Equal(receiver, session.Known(array), Origin{})}
		if reverse {
			facts[0], facts[1] = facts[1], facts[0]
		}
		for _, fact := range facts {
			session.Add(fact)
		}
		slot := session.PublishSlot(result)
		solution := session.Solve()
		resolved, _ := solution.Slot(slot)
		key, _ := store.Key(resolved.Type)
		return key
	}
	forward, reverse := run(false), run(true)
	forwardElement, forwardOK := forward.Child()
	reverseElement, reverseOK := reverse.Child()
	if forward.Kind() != types.Slice || reverse.Kind() != types.Slice || !forwardOK || !reverseOK || forwardElement != b.I64 || reverseElement != b.I64 {
		t.Fatalf("forward=%+v reverse=%+v", forward, reverse)
	}
}

func TestNewSlotAndStructuralAPIsAreRaceFreeAcrossSessions(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	array := mustType(t, store, types.ArrayKey(8, b.Char))
	var wait sync.WaitGroup
	for range 8 {
		wait.Add(1)
		go func() {
			defer wait.Done()
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			result := session.Variable(Origin{})
			session.Add(Sliceable(session.Known(array), result, Origin{}))
			slot := session.PublishSlot(result)
			solution := session.Solve()
			got, ok := solution.Slot(slot)
			if !solution.Successful() || !ok || got.State != TypeFinal || diagnostics.HasErrors() {
				t.Errorf("solution=%v result=%+v diagnostics=%+v", solution.Successful(), got, diagnostics.Items())
			}
		}()
	}
	wait.Wait()
}

func TestConcurrentFirstInternAndKnownConflictStoreAccess(t *testing.T) {
	program, store := testProgram(t)
	b := store.Builtins()
	array := mustType(t, store, types.ArrayKey(19, b.I32))
	start := make(chan struct{})
	var wait sync.WaitGroup
	for index := range 16 {
		wait.Add(1)
		go func() {
			defer wait.Done()
			<-start
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			if index%2 == 0 {
				result := session.Variable(Origin{})
				session.Add(Sliceable(session.Known(array), result, Origin{}))
				slot := session.PublishSlot(result)
				solution := session.Solve()
				got, ok := solution.Slot(slot)
				if !solution.Successful() || !ok || got.State != TypeFinal || diagnostics.HasErrors() {
					t.Errorf("intern solution=%v result=%+v diagnostics=%+v", solution.Successful(), got, diagnostics.Items())
				}
				return
			}
			session.Add(Equal(session.Known(b.Bool), session.Known(b.Int), Origin{}))
			solution := session.Solve()
			if solution.Successful() || !hasDiagnostic(diagnostics, CodeUnification) {
				t.Errorf("conflict solution=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
			}
		}()
	}
	close(start)
	wait.Wait()
}

func assertSlotType(t *testing.T, solution *Solution, slot SlotID, expected types.TypeID) {
	t.Helper()
	result, ok := solution.Slot(slot)
	if !ok || result.State != TypeFinal || result.Type != expected {
		t.Fatalf("slot result=(%+v,%v), want type %d", result, ok, expected)
	}
}

func assertSlotError(t *testing.T, solution *Solution, slot SlotID) {
	t.Helper()
	result, ok := solution.Slot(slot)
	if !ok || result.State != TypeError || result.Type != 0 {
		t.Fatalf("slot result=(%+v,%v), want error", result, ok)
	}
}

func mustType(t *testing.T, store *types.Store, key types.TypeKey) types.TypeID {
	t.Helper()
	id, err := store.Intern(key)
	if err != nil {
		t.Fatal(err)
	}
	return id
}
