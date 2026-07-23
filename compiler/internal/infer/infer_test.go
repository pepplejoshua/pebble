package infer

import (
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func testProgram(t *testing.T) (*Program, *types.Store) {
	t.Helper()
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	return &Program{valid: true, inputs: ProgramInputs{Types: store, LiteralTarget: LiteralTarget{WordBits: 64}}, declarations: map[symbol.SymbolID]TypeDeclaration{}, typeParams: map[symbol.SymbolID]types.TypeID{}}, store
}

func TestEquationOrderDoesNotChooseTypes(t *testing.T) {
	run := func(reverse bool) (types.TypeID, []diagnostic.Code) {
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
		var codes []diagnostic.Code
		for _, item := range diagnostics.Items() {
			codes = append(codes, item.Code)
		}
		return got.Type, codes
	}
	a, ac := run(false)
	b, bc := run(true)
	if a != b || len(ac) != 0 || len(bc) != 0 {
		t.Fatalf("ordered solve = (%d,%v), reversed = (%d,%v)", a, ac, b, bc)
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
	if first != second {
		t.Fatal("repeated Solve must preserve the frozen solution")
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
