package infer

import (
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestRuntimePreludeNominalsMembersAndSourceResolution(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`fn preserve(value Allocator) Allocator => value;`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	runtimeTypes, ok := program.RuntimeTypes()
	if !ok {
		t.Fatal("runtime types are unavailable")
	}
	allocatorSymbol, _ := program.inputs.Resolution.Runtime(symbol.RuntimeAllocator)
	contextSymbol, _ := program.inputs.Resolution.Runtime(symbol.RuntimeContext)
	assertNominalIdentity(t, program.inputs.Types, runtimeTypes.Allocator, allocatorSymbol)
	assertNominalIdentity(t, program.inputs.Types, runtimeTypes.Context, contextSymbol)

	builtins := program.inputs.Types.Builtins()
	voidPointer, _ := program.inputs.Types.Intern(types.PointerKey(builtins.Void))
	alloc, _ := program.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, builtins.Uint}, voidPointer, false))
	realloc, _ := program.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer, builtins.Uint}, voidPointer, false))
	free, _ := program.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{voidPointer, voidPointer}, builtins.Void, false))
	assertRuntimeMembers(t, program, allocatorSymbol, []string{"ptr", "alloc", "realloc", "free"}, []types.TypeID{voidPointer, alloc, realloc, free})
	assertRuntimeMembers(t, program, contextSymbol, []string{"default_allocator"}, []types.TypeID{runtimeTypes.Allocator})

	for _, callback := range []struct {
		id         types.TypeID
		parameters []types.TypeID
		result     types.TypeID
	}{
		{alloc, []types.TypeID{voidPointer, builtins.Uint}, voidPointer},
		{realloc, []types.TypeID{voidPointer, voidPointer, builtins.Uint}, voidPointer},
		{free, []types.TypeID{voidPointer, voidPointer}, builtins.Void},
	} {
		key, _ := program.inputs.Types.Key(callback.id)
		convention, parameters, result, variadic, ok := key.Function()
		if !ok || convention != types.Pebble || variadic || result != callback.result || !equalTypeIDs(parameters, callback.parameters) {
			t.Fatalf("callback key=%+v convention=%d parameters=%v result=%d variadic=%v", key, convention, parameters, result, variadic)
		}
	}

	var allocatorOccurrence symbol.SyntaxRef
	for _, reference := range program.inputs.Resolution.References() {
		if reference.Symbol == allocatorSymbol {
			allocatorOccurrence = reference.Syntax
			break
		}
	}
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	resolved := session.ResolveType(allocatorOccurrence, 0)
	if resolved.State != TypeFinal || resolved.Type != runtimeTypes.Allocator {
		t.Fatalf("Allocator resolution = %+v", resolved)
	}
	if !session.Solve().Successful() {
		t.Fatal("source-resolution session failed")
	}

	second, secondDiagnostics := prepareSource(t, []byte(`fn preserve(value Allocator) Allocator => value;`))
	secondRuntime, secondOK := second.RuntimeTypes()
	secondAllocator, _ := second.inputs.Resolution.Runtime(symbol.RuntimeAllocator)
	secondContext, _ := second.inputs.Resolution.Runtime(symbol.RuntimeContext)
	if secondDiagnostics.HasErrors() || !secondOK {
		t.Fatalf("second runtime preparation failed: types=%+v diagnostics=%+v", secondRuntime, secondDiagnostics.Items())
	}
	assertNominalIdentity(t, second.inputs.Types, secondRuntime.Allocator, secondAllocator)
	assertNominalIdentity(t, second.inputs.Types, secondRuntime.Context, secondContext)
	secondBuiltins := second.inputs.Types.Builtins()
	secondVoidPointer, _ := second.inputs.Types.Intern(types.PointerKey(secondBuiltins.Void))
	secondAlloc, _ := second.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{secondVoidPointer, secondBuiltins.Uint}, secondVoidPointer, false))
	secondRealloc, _ := second.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{secondVoidPointer, secondVoidPointer, secondBuiltins.Uint}, secondVoidPointer, false))
	secondFree, _ := second.inputs.Types.Intern(types.FunctionKey(types.Pebble, []types.TypeID{secondVoidPointer, secondVoidPointer}, secondBuiltins.Void, false))
	assertRuntimeMembers(t, second, secondAllocator, []string{"ptr", "alloc", "realloc", "free"}, []types.TypeID{secondVoidPointer, secondAlloc, secondRealloc, secondFree})
	assertRuntimeMembers(t, second, secondContext, []string{"default_allocator"}, []types.TypeID{secondRuntime.Allocator})
}

func TestRuntimePreludeHasFieldAndConcurrentSessions(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`fn preserve(value Allocator) Allocator => value;`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	runtimeTypes, _ := program.RuntimeTypes()
	allocatorSymbol, _ := program.inputs.Resolution.Runtime(symbol.RuntimeAllocator)
	contextSymbol, _ := program.inputs.Resolution.Runtime(symbol.RuntimeContext)
	allocatorExpected := runtimeMemberTypes(t, program, allocatorSymbol)
	contextExpected := runtimeMemberTypes(t, program, contextSymbol)

	run := func(local *diagnostic.DiagnosticSet) {
		session := NewSession(program, local, Config{})
		node := symbol.SyntaxRef{Module: 1, Node: 1}
		for index, name := range []string{"ptr", "alloc", "realloc", "free"} {
			field := session.Variable(Origin{Role: name})
			session.Add(HasField(session.Known(runtimeTypes.Allocator), name, field, Origin{Role: name}))
			ref := node
			ref.Node += syntax.NodeID(index)
			session.PublishSyntax(ref, field)
		}
		contextField := session.Variable(Origin{Role: "default_allocator"})
		session.Add(HasField(session.Known(runtimeTypes.Context), "default_allocator", contextField, Origin{Role: "default_allocator"}))
		contextRef := symbol.SyntaxRef{Module: 1, Node: 20}
		session.PublishSyntax(contextRef, contextField)
		solution := session.Solve()
		if !solution.Successful() || local.HasErrors() {
			t.Errorf("runtime HasField failed: %+v", local.Items())
			return
		}
		for index := range allocatorExpected {
			ref := node
			ref.Node += syntax.NodeID(index)
			got, _ := solution.SyntaxType(ref)
			if got.Type != allocatorExpected[index] {
				t.Errorf("Allocator field %d type=%d want=%d", index, got.Type, allocatorExpected[index])
			}
		}
		got, _ := solution.SyntaxType(contextRef)
		if got.Type != contextExpected[0] {
			t.Errorf("Context.default_allocator type=%d want=%d", got.Type, contextExpected[0])
		}
	}

	run(diagnostic.NewDiagnosticSet())
	var wait sync.WaitGroup
	for range 8 {
		wait.Add(1)
		go func() {
			defer wait.Done()
			run(diagnostic.NewDiagnosticSet())
		}()
	}
	wait.Wait()
}

func TestDamagedRuntimePreludeDoesNotPublishTypes(t *testing.T) {
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	provider := inferenceMemoryProvider{"main.peb": []byte(`fn valid() void {}`)}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "types"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{MaxSymbols: 16})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	before := store.Len()
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	if runtimeTypes, ok := program.RuntimeTypes(); ok || runtimeTypes != (RuntimeTypes{}) {
		t.Fatalf("damaged runtime types = %+v, %v", runtimeTypes, ok)
	}
	if store.Len() != before || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("store len %d -> %d diagnostics=%+v", before, store.Len(), diagnostics.Items())
	}
}

func TestDamagedRuntimePreludeAllocatorOccurrenceDoesNotEmitTypeDiagnostic(t *testing.T) {
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	provider := inferenceMemoryProvider{"main.peb": []byte(`fn preserve(value Allocator) Allocator => value;`)}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "types"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	allocator, ok := resolution.Runtime(symbol.RuntimeAllocator)
	if !ok {
		t.Fatal("valid 04b result is missing Allocator")
	}
	foundOccurrence := false
	for _, reference := range resolution.References() {
		if reference.Symbol == allocator && reference.State == symbol.ResolutionResolved {
			foundOccurrence = true
			break
		}
	}
	if !foundOccurrence {
		t.Fatal("valid authored Allocator occurrence was not resolved by 04b")
	}
	store, err := types.New(types.Config{MaxTypes: 16})
	if err != nil {
		t.Fatal(err)
	}
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	if _, ready := program.RuntimeTypes(); ready {
		t.Fatal("runtime prelude unexpectedly succeeded against the limited store")
	}
	resourceDiagnostics, typeDiagnostics := 0, 0
	for _, item := range diagnostics.Items() {
		switch item.Code {
		case CodeResourceLimit:
			resourceDiagnostics++
		case CodeInvalidType:
			typeDiagnostics++
		}
	}
	if resourceDiagnostics != 2 || typeDiagnostics != 0 {
		t.Fatalf("runtime diagnostics: T0512=%d T0501=%d items=%+v", resourceDiagnostics, typeDiagnostics, diagnostics.Items())
	}
}

func assertNominalIdentity(t *testing.T, store *types.Store, id types.TypeID, declaration symbol.SymbolID) {
	t.Helper()
	key, ok := store.Key(id)
	if !ok {
		t.Fatalf("type %d is not store-owned", id)
	}
	gotDeclaration, arguments, ok := key.Nominal()
	if !ok || gotDeclaration != declaration || len(arguments) != 0 {
		t.Fatalf("nominal type %d = declaration %d arguments %v", id, gotDeclaration, arguments)
	}
}

func assertRuntimeMembers(t *testing.T, program *Program, owner symbol.SymbolID, names []string, expected []types.TypeID) {
	t.Helper()
	declaration, ok := program.TypeDeclaration(owner)
	if !ok || declaration.State != DeclarationReady || declaration.Form != DeclarationNominal || len(declaration.Members) != len(names) {
		t.Fatalf("runtime declaration %d = %+v", owner, declaration)
	}
	for index, member := range declaration.Members {
		value, _ := program.inputs.Resolution.Symbols.Symbol(member.Symbol)
		template, templateOK := program.Template(member.Type)
		if value.Name != names[index] || value.Containing != owner || !templateOK || template.Kind != TemplateKnown || template.Known != expected[index] {
			t.Fatalf("member %d = %+v symbol=%+v template=%+v", index, member, value, template)
		}
	}
}

func runtimeMemberTypes(t *testing.T, program *Program, owner symbol.SymbolID) []types.TypeID {
	t.Helper()
	declaration, _ := program.TypeDeclaration(owner)
	result := make([]types.TypeID, len(declaration.Members))
	for index, member := range declaration.Members {
		template, _ := program.Template(member.Type)
		result[index] = template.Known
	}
	return result
}

func equalTypeIDs(a, b []types.TypeID) bool {
	if len(a) != len(b) {
		return false
	}
	for index := range a {
		if a[index] != b[index] {
			return false
		}
	}
	return true
}

func testProgram(t *testing.T) (*Program, *types.Store) {
	t.Helper()
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	return &Program{storeMu: &sync.Mutex{}, valid: true, inputs: ProgramInputs{Types: store, LiteralTarget: LiteralTarget{WordBits: 64}}, declarations: map[symbol.SymbolID]TypeDeclaration{}, typeParams: map[symbol.SymbolID]types.TypeID{}}, store
}

func TestProgramTypeParameter(t *testing.T) {
	parameter := symbol.SymbolID(41)
	rigid := types.TypeID(0xfedcba98)
	program := &Program{typeParams: map[symbol.SymbolID]types.TypeID{parameter: rigid}}

	if got, ok := program.TypeParameter(parameter); !ok || got != rigid {
		t.Fatalf("TypeParameter(%d) = %d, %v; want exact %d, true", parameter, got, ok, rigid)
	}
	for _, id := range []symbol.SymbolID{0, 42, 9001} {
		if got, ok := program.TypeParameter(id); ok || got != 0 {
			t.Errorf("TypeParameter(%d) = %d, %v; want 0, false", id, got, ok)
		}
	}
	var nilProgram *Program
	if got, ok := nilProgram.TypeParameter(parameter); ok || got != 0 {
		t.Fatalf("nil Program TypeParameter(%d) = %d, %v; want 0, false", parameter, got, ok)
	}

	if allocations := testing.AllocsPerRun(100, func() {
		got, ok := program.TypeParameter(parameter)
		if !ok || got != rigid {
			panic("TypeParameter changed during repeated reads")
		}
	}); allocations != 0 {
		t.Fatalf("TypeParameter allocated %v times per read", allocations)
	}

	var wait sync.WaitGroup
	for range 16 {
		wait.Add(1)
		go func() {
			defer wait.Done()
			for range 100 {
				if got, ok := program.TypeParameter(parameter); !ok || got != rigid {
					t.Errorf("concurrent TypeParameter(%d) = %d, %v; want %d, true", parameter, got, ok, rigid)
					return
				}
			}
		}()
	}
	wait.Wait()
}

func TestEquationOrderDoesNotChooseTypes(t *testing.T) {
	run := func(reverse bool) []diagnostic.Code {
		program, store := testProgram(t)
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		a, b := session.Variable(Origin{Role: "a"}), session.Variable(Origin{Role: "b"})
		integer := session.Known(store.Builtins().I32)
		facts := []Constraint{Equal(a, b, Origin{Role: "link"}), Equal(b, integer, Origin{Role: "expected"})}
		if reverse {
			facts[0], facts[1] = facts[1], facts[0]
		}
		for _, fact := range facts {
			session.Add(fact)
		}
		session.PublishSymbol(1, a)
		solution := session.Solve()
		got, _ := solution.SymbolType(1)
		if got.State != TypeFinal || got.Type != store.Builtins().I32 {
			t.Fatalf("reverse=%v result=%+v", reverse, got)
		}
		var codes []diagnostic.Code
		for _, item := range diagnostics.Items() {
			codes = append(codes, item.Code)
		}
		return codes
	}
	forward := run(false)
	reverse := run(true)
	if len(forward) != 0 || len(reverse) != 0 {
		t.Fatalf("ordered diagnostics=%v reversed diagnostics=%v", forward, reverse)
	}
}

func TestVariableChainsAndStructuralUnification(t *testing.T) {
	program, store := testProgram(t)
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	a, b, c := session.Variable(Origin{}), session.Variable(Origin{}), session.Variable(Origin{})
	session.Add(Equal(c, b, Origin{}))
	session.Add(Equal(a, c, Origin{}))
	session.Add(ConstrainShape(b, OptionalShape(Leaf(session.Known(store.Builtins().I16))), Origin{}))
	session.PublishSymbol(1, a)
	solution := session.Solve()
	want, _ := store.Intern(types.OptionalKey(store.Builtins().I16))
	got, _ := solution.SymbolType(1)
	if !solution.Successful() || got.Type != want || session.cells[session.find(a.id)-1].minimum != a.id {
		t.Fatalf("solution=%+v successful=%v", got, solution.Successful())
	}
}

func TestOccursCheckRecoversAtomically(t *testing.T) {
	program, _ := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	v := session.Variable(Origin{Role: "recursive"})
	session.Add(ConstrainShape(v, PointerShape(Leaf(v)), Origin{Role: "pointer"}))
	session.PublishSymbol(1, v)
	solution := session.Solve()
	if solution.Successful() || !hasDiagnostic(diagnostics, CodeOccursCheck) {
		t.Fatalf("want occurs-check failure, got %+v", diagnostics.Items())
	}
}

func TestExactLiteralFittingAndDefaulting(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	unconstrained := session.IntegerLiteral([]byte("08"), Origin{Role: "default"})
	negative := session.NegateLiteral(session.IntegerLiteral([]byte("128"), Origin{}), Origin{Role: "negative"})
	session.Add(LiteralFits(negative, session.Known(store.Builtins().I8), Origin{Role: "fit"}))
	session.PublishSymbol(1, unconstrained)
	session.PublishSymbol(2, negative)
	solution := session.Solve()
	first, _ := solution.SymbolType(1)
	second, _ := solution.SymbolType(2)
	if !solution.Successful() || first.Type != store.Builtins().Int || second.Type != store.Builtins().I8 {
		t.Fatalf("default=%+v fit=%+v diagnostics=%+v", first, second, diagnostics.Items())
	}
}

func TestLiteralOverflowAndCapabilityFailure(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	tooLarge := session.IntegerLiteral([]byte("256"), Origin{})
	session.Add(LiteralFits(tooLarge, session.Known(store.Builtins().U8), Origin{}))
	session.Add(Integral(session.Known(store.Builtins().F32), Origin{}))
	session.Solve()
	if !hasDiagnostic(diagnostics, CodeLiteral) || !hasDiagnostic(diagnostics, CodeCapability) {
		t.Fatalf("missing fitting/capability diagnostics: %+v", diagnostics.Items())
	}
}

func TestExpectedResultAndGenericInstantiation(t *testing.T) {
	program, store := testProgram(t)
	parameter := symbol.SymbolID(90)
	rigid, _ := store.Intern(types.TypeParameterKey(parameter))
	program.typeParams[parameter] = rigid
	p := program.addTemplate(TypeTemplate{Kind: TemplateParameter, Parameter: parameter})
	optional := program.addTemplate(TypeTemplate{Kind: TemplateOptional, Children: []TemplateID{p}})
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	argument := session.Variable(Origin{})
	result := session.Variable(Origin{})
	session.Add(Equal(argument, session.Known(store.Builtins().Char), Origin{Role: "argument"}))
	session.Add(Instantiate(optional, []Substitution{{Parameter: parameter, Argument: argument}}, result, Origin{}))
	want, _ := store.Intern(types.OptionalKey(store.Builtins().Char))
	session.Add(Equal(result, session.Known(want), Origin{Role: "expected result"}))
	session.PublishInstantiation(symbol.SyntaxRef{Module: 1, Node: 1}, 50, []Term{argument})
	session.PublishSyntax(symbol.SyntaxRef{Module: 1, Node: 1}, result)
	solution := session.Solve()
	inst, _ := solution.Instantiation(symbol.SyntaxRef{Module: 1, Node: 1})
	got, _ := solution.SyntaxType(symbol.SyntaxRef{Module: 1, Node: 1})
	if !solution.Successful() || got.Type != want || len(inst.Arguments) != 1 || inst.Arguments[0].Type != store.Builtins().Char {
		t.Fatalf("result=%+v instantiation=%+v", got, inst)
	}
}

func TestRigidCapabilityBecomesRequirement(t *testing.T) {
	program, store := testProgram(t)
	parameter := symbol.SymbolID(12)
	rigid, _ := store.Intern(types.TypeParameterKey(parameter))
	program.typeParams[parameter] = rigid
	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	session.Add(Numeric(session.Known(rigid), Origin{GenericOwner: 20, Role: "operator"}))
	solution := session.Solve()
	requirements := solution.Requirements(20)
	if !solution.Successful() || len(requirements) != 1 || requirements[0].Kind != RequirementNumeric || requirements[0].Subject != rigid {
		t.Fatalf("requirements=%+v", requirements)
	}
}

func TestRigidRequirementsAreEquationOrderIndependent(t *testing.T) {
	run := func(reverse bool) []Requirement {
		program, store := testProgram(t)
		parameter := symbol.SymbolID(12)
		rigid, _ := store.Intern(types.TypeParameterKey(parameter))
		program.typeParams[parameter] = rigid
		session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
		term := session.Variable(Origin{GenericOwner: 20, Role: "generic value"})
		literal := session.IntegerLiteral([]byte("255"), Origin{GenericOwner: 20, Role: "literal"})
		facts := []Constraint{
			Numeric(term, Origin{GenericOwner: 20, Role: "operator"}),
			Equal(term, session.Known(rigid), Origin{GenericOwner: 20, Role: "rigid equality"}),
			LiteralFits(literal, term, Origin{GenericOwner: 20, Role: "literal fit"}),
		}
		if reverse {
			facts[0], facts[2] = facts[2], facts[0]
		}
		for _, fact := range facts {
			session.Add(fact)
		}
		solution := session.Solve()
		if !solution.Successful() {
			t.Fatalf("reverse=%v failed", reverse)
		}
		return solution.Requirements(20)
	}
	forward, reverse := run(false), run(true)
	if len(forward) != 2 || len(reverse) != 2 || forward[0].Kind != RequirementNumeric || forward[1].Kind != RequirementLiteralFits || reverse[0].Kind != RequirementNumeric || reverse[1].Kind != RequirementLiteralFits || forward[1].Numerator != "255" || reverse[1].Numerator != "255" {
		t.Fatalf("forward=%+v reverse=%+v", forward, reverse)
	}
}

func TestOneOfUniqueAmbiguousAndRollback(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	v := session.Variable(Origin{})
	session.Add(Equal(v, session.Known(store.Builtins().Bool), Origin{}))
	id := session.Add(OneOf([]Alternative{
		{Label: "numeric", Constraints: []Constraint{Equal(v, session.Known(store.Builtins().Int), Origin{})}},
		{Label: "boolean", Constraints: []Constraint{Equal(v, session.Known(store.Builtins().Bool), Origin{})}},
	}, Origin{}))
	solution := session.Solve()
	selected, ok := solution.Selection(id)
	if !solution.Successful() || !ok || selected != 1 {
		t.Fatalf("selection=(%d,%v), diagnostics=%+v", selected, ok, diagnostics.Items())
	}

	program2, store2 := testProgram(t)
	diagnostics2 := diagnostic.NewDiagnosticSet()
	session2 := NewSession(program2, diagnostics2, Config{})
	w := session2.Variable(Origin{})
	session2.Add(OneOf([]Alternative{
		{Label: "first", Constraints: []Constraint{Equal(w, session2.Known(store2.Builtins().Int), Origin{})}},
		{Label: "second", Constraints: []Constraint{Equal(w, session2.Known(store2.Builtins().I64), Origin{})}},
	}, Origin{}))
	session2.Solve()
	if !hasDiagnostic(diagnostics2, CodeAmbiguous) {
		t.Fatalf("want ambiguity, got %+v", diagnostics2.Items())
	}

	program3, store3 := testProgram(t)
	diagnostics3 := diagnostic.NewDiagnosticSet()
	session3 := NewSession(program3, diagnostics3, Config{})
	x := session3.Variable(Origin{})
	session3.Add(Equal(x, session3.Known(store3.Builtins().Bool), Origin{}))
	session3.Add(OneOf([]Alternative{
		{Label: "integer", Constraints: []Constraint{Equal(x, session3.Known(store3.Builtins().Int), Origin{Role: "first conflict"})}},
		{Label: "float", Constraints: []Constraint{Equal(x, session3.Known(store3.Builtins().F64), Origin{Role: "second conflict"})}},
	}, Origin{}))
	session3.Solve()
	if !hasDiagnostic(diagnostics3, CodeUnification) || hasDiagnostic(diagnostics3, CodeAmbiguous) {
		t.Fatalf("zero-viability choice must publish its first branch conflict: %+v", diagnostics3.Items())
	}
}

func TestInterdependentChoicesResolveAsOneGlobalSystem(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	x := session.Variable(Origin{})
	first := session.Add(OneOf([]Alternative{
		{Label: "int", Constraints: []Constraint{Equal(x, session.Known(store.Builtins().Int), Origin{})}},
		{Label: "bool", Constraints: []Constraint{Equal(x, session.Known(store.Builtins().Bool), Origin{})}},
	}, Origin{Role: "first choice"}))
	second := session.Add(OneOf([]Alternative{
		{Label: "int", Constraints: []Constraint{Equal(x, session.Known(store.Builtins().Int), Origin{})}},
		{Label: "float", Constraints: []Constraint{Equal(x, session.Known(store.Builtins().F64), Origin{})}},
	}, Origin{Role: "second choice"}))
	session.PublishSymbol(1, x)
	solution := session.Solve()
	firstSelection, firstOK := solution.Selection(first)
	secondSelection, secondOK := solution.Selection(second)
	result, _ := solution.SymbolType(1)
	if !solution.Successful() || !firstOK || !secondOK || firstSelection != 0 || secondSelection != 0 || result.Type != store.Builtins().Int || diagnostics.HasErrors() {
		t.Fatalf("first=(%d,%v) second=(%d,%v) result=%+v diagnostics=%+v", firstSelection, firstOK, secondSelection, secondOK, result, diagnostics.Items())
	}
}

func TestDelayedGenericMethodSelectionUsesReceiverArguments(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`
type Box[T] = struct {
    fn map[U](self Box[T], value U) U => value;
};
`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	var box, method symbol.SymbolID
	for _, candidate := range program.inputs.Resolution.Symbols.All() {
		switch {
		case candidate.Kind == symbol.SymbolType && candidate.Name == "Box":
			box = candidate.ID
		case candidate.Kind == symbol.SymbolMethod && candidate.Name == "map":
			method = candidate.ID
		}
	}
	if box == 0 || method == 0 {
		t.Fatalf("box=%d method=%d", box, method)
	}
	store := program.inputs.Types
	receiverID, err := store.Intern(types.NominalKey(box, []types.TypeID{store.Builtins().Int}))
	if err != nil {
		t.Fatal(err)
	}
	session := NewSession(program, diagnostics, Config{})
	receiver := session.Known(receiverID)
	callable := session.Variable(Origin{Role: "method callable"})
	result := session.Variable(Origin{Role: "call result"})
	session.Add(ConstrainShape(callable, FunctionShape(types.Pebble, []Shape{
		Leaf(receiver), Leaf(session.Known(store.Builtins().Char)),
	}, Leaf(result), false), Origin{Role: "call evidence"}))
	site := symbol.SyntaxRef{Module: 1, Node: 1}
	session.Add(SelectMethod(receiver, "map", callable, nil, site, Origin{Role: "method selection"}))
	session.PublishSyntax(site, result)
	solution := session.Solve()
	selected, ok := solution.Method(site)
	resultType, _ := solution.SyntaxType(site)
	if !solution.Successful() || !ok || selected.Method != method || len(selected.Arguments) != 1 || selected.Arguments[0].Type != store.Builtins().Char || resultType.Type != store.Builtins().Char {
		signature, _ := program.Signature(method)
		declaration, _ := program.TypeDeclaration(box)
		t.Fatalf("selection=%+v result=%+v declaration=%+v signature=%+v templates=%+v diagnostics=%+v", selected, resultType, declaration, signature, program.templates, diagnostics.Items())
	}
}

func TestPreparedProgramSupportsIndependentConcurrentSessions(t *testing.T) {
	program, diagnostics := prepareSource(t, []byte(`type Alias = ?*int; fn use(value Alias) Alias => value;`))
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}
	var occurrence symbol.SyntaxRef
	for _, reference := range program.inputs.Resolution.References() {
		selected, ok := program.inputs.Resolution.Symbols.Symbol(reference.Symbol)
		if ok && selected.Kind == symbol.SymbolBuiltinType && selected.Builtin == symbol.BuiltinInt {
			occurrence = reference.Syntax
			break
		}
	}
	if occurrence.Node == 0 {
		t.Fatal("missing builtin type occurrence")
	}
	var wait sync.WaitGroup
	for i := 0; i < 8; i++ {
		wait.Add(1)
		go func() {
			defer wait.Done()
			localDiagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, localDiagnostics, Config{})
			result := session.ResolveType(occurrence, 0)
			solution := session.Solve()
			if result.State != TypeFinal || result.Type != program.inputs.Types.Builtins().Int || !solution.Successful() || localDiagnostics.HasErrors() {
				t.Errorf("result=%+v successful=%v diagnostics=%+v", result, solution.Successful(), localDiagnostics.Items())
			}
		}()
	}
	wait.Wait()
}

func TestLimitsAndRepeatSolveAreBounded(t *testing.T) {
	program, _ := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{MaxInferVariables: 1, MaxDiagnostics: 2})
	session.Variable(Origin{})
	session.Variable(Origin{})
	first := session.Solve()
	second := session.Solve()
	if first == second || !first.finalized || second.finalized {
		t.Fatal("repeated Solve must return a distinct rejected recovery without changing the first solution")
	}
	if diagnostics.Len() > 2 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("bounded diagnostics=%+v", diagnostics.Items())
	}
}

func TestErrorTermSuppressesDependentConstraints(t *testing.T) {
	program, store := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	recovery := session.Error(Origin{Role: "damaged expression"})
	session.Add(Equal(recovery, session.Known(store.Builtins().Bool), Origin{Role: "dependent equality"}))
	session.PublishSyntax(symbol.SyntaxRef{Module: 1, Node: 1}, recovery)
	solution := session.Solve()
	result, ok := solution.SyntaxType(symbol.SyntaxRef{Module: 1, Node: 1})
	if solution.Successful() || !ok || result.State != TypeError || diagnostics.HasErrors() {
		t.Fatalf("solution=%+v result=%+v diagnostics=%+v", solution, result, diagnostics.Items())
	}
}

func hasDiagnostic(set *diagnostic.DiagnosticSet, code diagnostic.Code) bool {
	for _, item := range set.Items() {
		if item.Code == code {
			return true
		}
	}
	return false
}

func FuzzExactLiteralParsingIsBounded(f *testing.F) {
	for _, seed := range []string{"0", "-128", "0xffff_ffff", "1.25e-4", "0x1.fp+3", "1e999999999999"} {
		f.Add(seed)
	}
	config := normalizeConfig(Config{MaxLiteralBytes: 64, MaxLiteralBits: 256, MaxLiteralExponent: 256})
	f.Fuzz(func(t *testing.T, text string) {
		if len(text) > 128 {
			text = text[:128]
		}
		_, _ = parseIntegerLiteral([]byte(text), config)
		_, _ = parseFloatLiteral([]byte(text), config)
	})
}
