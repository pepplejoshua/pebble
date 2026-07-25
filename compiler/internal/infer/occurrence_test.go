package infer

import (
	"reflect"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func occurrenceRef(t *testing.T, program *Program, name string) symbol.SyntaxRef {
	t.Helper()
	for _, reference := range program.inputs.Resolution.References() {
		selected, ok := program.inputs.Resolution.Symbols.Symbol(reference.Symbol)
		if ok && selected.Name == name {
			return reference.Syntax
		}
	}
	t.Fatalf("missing occurrence for %q", name)
	return symbol.SyntaxRef{}
}

func pathOccurrenceRef(t *testing.T, program *Program, name string) symbol.SyntaxRef {
	t.Helper()
	for moduleID, item := range program.modules {
		var visit func(syntax.NodeID) symbol.SyntaxRef
		visit = func(id syntax.NodeID) symbol.SyntaxRef {
			node, ok := item.Tree.Node(id)
			if !ok {
				return symbol.SyntaxRef{}
			}
			if node.Kind() == syntax.Path {
				children := semanticNodeIDs(item.Tree, node.Children())
				if len(children) != 0 {
					ref := symbol.SyntaxRef{Module: moduleID, Node: children[len(children)-1]}
					resolved, found := program.inputs.Resolution.Reference(ref)
					selected, present := program.inputs.Resolution.Symbols.Symbol(resolved.Symbol)
					if found && present && selected.Name == name {
						return symbol.SyntaxRef{Module: moduleID, Node: id}
					}
				}
			}
			for _, child := range node.Children() {
				if found := visit(child); found.Node != 0 {
					return found
				}
			}
			return symbol.SyntaxRef{}
		}
		if found := visit(item.Tree.Root()); found.Node != 0 {
			return found
		}
	}
	t.Fatalf("missing path occurrence for %q", name)
	return symbol.SyntaxRef{}
}

func bracketArgumentContaining(t *testing.T, program *Program, target symbol.SyntaxRef) symbol.SyntaxRef {
	t.Helper()
	for moduleID, item := range program.modules {
		var contains func(syntax.NodeID) bool
		contains = func(id syntax.NodeID) bool {
			if moduleID == target.Module && id == target.Node {
				return true
			}
			node, ok := item.Tree.Node(id)
			if !ok {
				return false
			}
			for _, child := range node.Children() {
				if contains(child) {
					return true
				}
			}
			return false
		}
		var visit func(syntax.NodeID) symbol.SyntaxRef
		visit = func(id syntax.NodeID) symbol.SyntaxRef {
			node, ok := item.Tree.Node(id)
			if !ok {
				return symbol.SyntaxRef{}
			}
			if node.Kind() == syntax.BracketApply {
				children := semanticNodeIDs(item.Tree, node.Children())
				for _, argument := range children[1:] {
					if contains(argument) {
						return symbol.SyntaxRef{Module: moduleID, Node: argument}
					}
				}
			}
			for _, child := range node.Children() {
				if found := visit(child); found.Node != 0 {
					return found
				}
			}
			return symbol.SyntaxRef{}
		}
		if found := visit(item.Tree.Root()); found.Node != 0 {
			return found
		}
	}
	t.Fatal("missing bracket argument containing target")
	return symbol.SyntaxRef{}
}

func occurrenceNodeKind(t *testing.T, program *Program, kind syntax.NodeKind) symbol.SyntaxRef {
	t.Helper()
	for moduleID, item := range program.modules {
		var visit func(syntax.NodeID) symbol.SyntaxRef
		visit = func(id syntax.NodeID) symbol.SyntaxRef {
			node, ok := item.Tree.Node(id)
			if !ok {
				return symbol.SyntaxRef{}
			}
			if node.Kind() == kind {
				return symbol.SyntaxRef{Module: moduleID, Node: id}
			}
			for _, child := range node.Children() {
				if found := visit(child); found.Node != 0 {
					return found
				}
			}
			return symbol.SyntaxRef{}
		}
		if found := visit(item.Tree.Root()); found.Node != 0 {
			return found
		}
	}
	t.Fatalf("missing occurrence node kind %s", kind)
	return symbol.SyntaxRef{}
}

func nameWithoutResolution(t *testing.T, program *Program) symbol.SyntaxRef {
	t.Helper()
	for moduleID, item := range program.modules {
		var visit func(syntax.NodeID) symbol.SyntaxRef
		visit = func(id syntax.NodeID) symbol.SyntaxRef {
			node, ok := item.Tree.Node(id)
			if !ok {
				return symbol.SyntaxRef{}
			}
			ref := symbol.SyntaxRef{Module: moduleID, Node: id}
			if node.Kind() == syntax.Name {
				if _, found := program.inputs.Resolution.Reference(ref); !found {
					return ref
				}
			}
			for _, child := range node.Children() {
				if found := visit(child); found.Node != 0 {
					return found
				}
			}
			return symbol.SyntaxRef{}
		}
		if found := visit(item.Tree.Root()); found.Node != 0 {
			return found
		}
	}
	t.Fatal("missing name without resolution evidence")
	return symbol.SyntaxRef{}
}

func treeNodeKind(t *testing.T, tree *syntax.Tree, kind syntax.NodeKind) syntax.NodeID {
	t.Helper()
	var visit func(syntax.NodeID) syntax.NodeID
	visit = func(id syntax.NodeID) syntax.NodeID {
		node, ok := tree.Node(id)
		if !ok {
			return 0
		}
		if node.Kind() == kind {
			return id
		}
		for _, child := range node.Children() {
			if found := visit(child); found != 0 {
				return found
			}
		}
		return 0
	}
	if found := visit(tree.Root()); found != 0 {
		return found
	}
	t.Fatalf("missing tree node kind %s", kind)
	return 0
}

func occurrenceChoice(t *testing.T, program *Program, ref symbol.SyntaxRef, subject types.TypeID, config Config) (*Solution, *diagnostic.DiagnosticSet, *Session, ConstraintID) {
	t.Helper()
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, config)
	known := session.Known(subject)
	id := session.Add(OneOf([]Alternative{
		{Label: "generic", Constraints: []Constraint{TypeOccurrence(ref, 0, known, Origin{Syntax: ref, Role: "type interpretation"})}},
		{Label: "runtime", Constraints: []Constraint{ValueOccurrence(ref, Origin{Syntax: ref, Role: "value interpretation"})}},
	}, Origin{Syntax: ref, Role: "deferred bracket"}))
	return session.Solve(), diagnostics, session, id
}

func TestOccurrenceConstraintsSelectRuntimeAndGenericInterpretations(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`
type TypeName = int;
type Container = struct { member int; };
fn use(container Container, valueSymbol int) void { container.member[valueSymbol]; container.member[TypeName]; }
`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}

	valueRef := occurrenceRef(t, program, "valueSymbol")
	valueSolution, valueDiagnostics, valueSession, valueChoice := occurrenceChoice(t, program, valueRef, program.inputs.Types.Builtins().Int, Config{})
	selected, ok := valueSolution.Selection(valueChoice)
	if !valueSolution.Successful() || !ok || selected != 1 || valueDiagnostics.HasErrors() {
		t.Fatalf("runtime selection=(%d,%v) successful=%v diagnostics=%+v", selected, ok, valueSolution.Successful(), valueDiagnostics.Items())
	}
	if len(valueSession.typeOccurrenceMemo) != 0 || len(valueSession.valueOccurrenceMemo) != 1 {
		t.Fatalf("losing type memo leaked: type=%d value=%d", len(valueSession.typeOccurrenceMemo), len(valueSession.valueOccurrenceMemo))
	}

	typeRef := occurrenceRef(t, program, "TypeName")
	typeSolution, typeDiagnostics, _, typeChoice := occurrenceChoice(t, program, typeRef, program.inputs.Types.Builtins().Int, Config{})
	selected, ok = typeSolution.Selection(typeChoice)
	if !typeSolution.Successful() || !ok || selected != 0 || typeDiagnostics.HasErrors() {
		t.Fatalf("generic selection=(%d,%v) successful=%v diagnostics=%+v", selected, ok, typeSolution.Successful(), typeDiagnostics.Items())
	}
}

func TestOccurrenceConstraintsSupportQualifiedTypeAndValueNames(t *testing.T) {
	provider := inferenceMemoryProvider{
		"main.peb": []byte(`import "./dep"; type Container=struct { member int; }; fn use(container Container) void { container.member[dep::valueSymbol]; container.member[dep::TypeName]; }`),
		"dep.peb":  []byte(`type TypeName = int; let valueSymbol int = 1;`),
	}
	diagnostics := diagnostic.NewDiagnosticSet()
	sources := source.NewFileSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "occurrence"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	program := Prepare(ProgramInputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, ArrayLengths: fixedArrayLengths{}, LiteralTarget: LiteralTarget{WordBits: 64}}, diagnostics, Config{})
	if diagnostics.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", diagnostics.Items())
	}

	valueSolution, valueDiagnostics, _, valueChoice := occurrenceChoice(t, program, pathOccurrenceRef(t, program, "valueSymbol"), store.Builtins().Int, Config{})
	typeSolution, typeDiagnostics, _, typeChoice := occurrenceChoice(t, program, pathOccurrenceRef(t, program, "TypeName"), store.Builtins().Int, Config{})
	valueSelected, valueOK := valueSolution.Selection(valueChoice)
	typeSelected, typeOK := typeSolution.Selection(typeChoice)
	if !valueSolution.Successful() || !typeSolution.Successful() || !valueOK || !typeOK || valueSelected != 1 || typeSelected != 0 || valueDiagnostics.HasErrors() || typeDiagnostics.HasErrors() {
		t.Fatalf("value=(%d,%v,%+v) type=(%d,%v,%+v)", valueSelected, valueOK, valueDiagnostics.Items(), typeSelected, typeOK, typeDiagnostics.Items())
	}
}

func TestOccurrenceConstraintsNeitherAmbiguousAndInactiveDiagnostics(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`type TypeName = int; type Container=struct { member int; }; fn use(container Container, valueSymbol int) void { container.member[valueSymbol]; container.member[TypeName]; }`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}
	valueRef := occurrenceRef(t, program, "valueSymbol")
	typeRef := occurrenceRef(t, program, "TypeName")

	t.Run("neither preserves T0501", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		choice := session.Add(OneOf([]Alternative{
			{Label: "type", Constraints: []Constraint{TypeOccurrence(valueRef, 0, session.Known(program.inputs.Types.Builtins().Int), Origin{Role: "type"})}},
			{Label: "value", Constraints: []Constraint{ValueOccurrence(typeRef, Origin{Role: "value"})}},
		}, Origin{}))
		solution := session.Solve()
		if solution.Successful() || choice == 0 || !hasDiagnostic(diagnostics, CodeInvalidType) || hasDiagnostic(diagnostics, CodeAmbiguous) {
			t.Fatalf("successful=%v diagnostics=%+v", solution.Successful(), diagnostics.Items())
		}
	})

	t.Run("ambiguous", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		session.Add(OneOf([]Alternative{
			{Label: "type", Constraints: []Constraint{TypeOccurrence(typeRef, 0, session.Known(program.inputs.Types.Builtins().Int), Origin{})}},
			{Label: "value", Constraints: []Constraint{ValueOccurrence(valueRef, Origin{})}},
		}, Origin{}))
		if session.Solve().Successful() || !hasDiagnostic(diagnostics, CodeAmbiguous) {
			t.Fatalf("diagnostics=%+v", diagnostics.Items())
		}
	})
}

func TestOccurrenceConstraintsFatalLimitsForeignInputsAndMemoization(t *testing.T) {
	program, preparation := prepareSource(t, []byte(`type TypeName = int; type Container=struct { member int; }; fn use(container Container, valueSymbol int) void { container.member[((valueSymbol))]; container.member[TypeName]; container.member[?int]; }`))
	if preparation.HasErrors() {
		t.Fatalf("prepare diagnostics: %+v", preparation.Items())
	}
	typeRef := occurrenceRef(t, program, "TypeName")

	t.Run("cold and memoized", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		known := session.Known(program.inputs.Types.Builtins().Int)
		session.Add(OneOf([]Alternative{
			{Label: "type", Constraints: []Constraint{TypeOccurrence(typeRef, 0, known, Origin{}), TypeOccurrence(typeRef, 0, known, Origin{})}},
			{Label: "wrong", Constraints: []Constraint{ValueOccurrence(typeRef, Origin{})}},
		}, Origin{}))
		before := program.inputs.Types.Len()
		if !session.Solve().Successful() || diagnostics.HasErrors() || len(session.typeOccurrenceMemo) != 1 || program.inputs.Types.Len() != before {
			t.Fatalf("memo=%d before=%d after=%d diagnostics=%+v", len(session.typeOccurrenceMemo), before, program.inputs.Types.Len(), diagnostics.Items())
		}
	})

	t.Run("losing composite type does not intern", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		compositeRef := occurrenceNodeKind(t, program, syntax.OptionalType)
		valueRef := occurrenceRef(t, program, "valueSymbol")
		before := program.inputs.Types.Len()
		choice := session.Add(OneOf([]Alternative{
			{Label: "conflicting composite type", Constraints: []Constraint{TypeOccurrence(compositeRef, 0, session.Known(program.inputs.Types.Builtins().Bool), Origin{})}},
			{Label: "runtime", Constraints: []Constraint{ValueOccurrence(valueRef, Origin{})}},
		}, Origin{}))
		solution := session.Solve()
		selected, ok := solution.Selection(choice)
		if !solution.Successful() || !ok || selected != 1 || diagnostics.HasErrors() || program.inputs.Types.Len() != before {
			t.Fatalf("selection=(%d,%v) before=%d after=%d diagnostics=%+v", selected, ok, before, program.inputs.Types.Len(), diagnostics.Items())
		}
	})

	t.Run("foreign owner", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		session.Add(TypeOccurrence(typeRef, ^symbol.SymbolID(0), session.Known(program.inputs.Types.Builtins().Int), Origin{}))
		if session.Solve().Successful() || !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("diagnostics=%+v", diagnostics.Items())
		}
	})

	t.Run("damaged syntax", func(t *testing.T) {
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{})
		session.Add(ValueOccurrence(symbol.SyntaxRef{Module: typeRef.Module, Node: ^syntax.NodeID(0)}, Origin{}))
		if session.Solve().Successful() || !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("diagnostics=%+v", diagnostics.Items())
		}
	})

	t.Run("lowered limit is fatal", func(t *testing.T) {
		valueRef := occurrenceRef(t, program, "valueSymbol")
		groupedRef := bracketArgumentContaining(t, program, valueRef)
		diagnostics := diagnostic.NewDiagnosticSet()
		session := NewSession(program, diagnostics, Config{MaxTypeSyntaxDepth: 1})
		choice := session.Add(OneOf([]Alternative{
			{Label: "limited", Constraints: []Constraint{ValueOccurrence(groupedRef, Origin{})}},
			{Label: "otherwise viable", Constraints: []Constraint{ValueOccurrence(valueRef, Origin{})}},
		}, Origin{}))
		solution := session.Solve()
		if solution.Successful() || !hasDiagnostic(diagnostics, CodeResourceLimit) {
			t.Fatalf("choice=%d successful=%v diagnostics=%+v", choice, solution.Successful(), diagnostics.Items())
		}
		if _, ok := solution.Selection(choice); ok {
			t.Fatal("fatal alternative failure selected another branch")
		}
	})

	t.Run("fatal result is exploration-order independent", func(t *testing.T) {
		valueRef := occurrenceRef(t, program, "valueSymbol")
		groupedRef := bracketArgumentContaining(t, program, valueRef)
		groupedNode, _, _ := program.node(groupedRef)
		fatalOrigin := Origin{Syntax: groupedRef, Span: groupedNode.Span(), Role: "fatal occurrence"}
		var baseline diagnostic.Diagnostic
		for _, reverse := range []bool{false, true} {
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{MaxTypeSyntaxDepth: 1})
			alternatives := []Alternative{
				{Label: "semantic rejection", Constraints: []Constraint{ValueOccurrence(typeRef, Origin{Role: "nonfatal occurrence"})}},
				{Label: "fatal", Constraints: []Constraint{ValueOccurrence(groupedRef, fatalOrigin)}},
			}
			if reverse {
				alternatives[0], alternatives[1] = alternatives[1], alternatives[0]
			}
			session.Add(OneOf(alternatives, Origin{}))
			if session.Solve().Successful() {
				t.Fatalf("reverse=%v diagnostics=%+v", reverse, diagnostics.Items())
			}
			items := diagnostics.Items()
			if len(items) != 1 {
				t.Fatalf("reverse=%v diagnostics=%+v", reverse, items)
			}
			expected := diagnostic.Diagnostic{
				Severity: diagnostic.Error,
				Code:     CodeResourceLimit,
				Message:  "value-occurrence depth limit exceeded",
				Primary:  diagnostic.Label{Span: fatalOrigin.Span, Message: fatalOrigin.Role},
			}
			if !reflect.DeepEqual(items[0], expected) {
				t.Fatalf("reverse=%v\n got: %#v\nwant: %#v", reverse, items[0], expected)
			}
			if reverse {
				if !reflect.DeepEqual(items[0], baseline) {
					t.Fatalf("reversed alternatives changed complete diagnostic\nforward: %#v\nreverse: %#v", baseline, items[0])
				}
			} else {
				baseline = items[0]
			}
		}
	})
}

func TestSpeculativeT0512DominatesAndPreservesCompleteDiagnostic(t *testing.T) {
	program, _ := testProgram(t)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	nonfatal := Origin{Span: source.NewSpan(1, 2, 3), Role: "earlier nonfatal"}
	fatal := Origin{Span: source.NewSpan(2, 4, 8), Role: "fatal primary"}
	related := []Origin{
		{Span: source.NewSpan(3, 9, 10), Role: "first related"},
		{Span: source.NewSpan(4, 11, 15), Role: "second related"},
	}

	session.speculative = true
	session.conflict(CodeInvalidType, "earlier category conflict", nonfatal)
	session.conflict(CodeResourceLimit, "exact fatal conflict", fatal, related...)
	captured := cloneConflict(session.speculativeConflict)
	if captured == nil || captured.code != CodeResourceLimit || captured.message != "exact fatal conflict" || captured.origin != fatal || !reflect.DeepEqual(captured.related, related) {
		t.Fatalf("captured=%+v", captured)
	}

	session.speculative = false
	session.failed = false
	session.conflict(captured.code, captured.message, captured.origin, captured.related...)
	session.reporter.flush()
	expected := diagnostic.Diagnostic{
		Severity: diagnostic.Error,
		Code:     CodeResourceLimit,
		Message:  "exact fatal conflict",
		Primary:  diagnostic.Label{Span: fatal.Span, Message: fatal.Role},
		Related: []diagnostic.Label{
			{Span: related[0].Span, Message: related[0].Role},
			{Span: related[1].Span, Message: related[1].Role},
		},
	}
	items := diagnostics.Items()
	if len(items) != 1 || !reflect.DeepEqual(items[0], expected) {
		t.Fatalf("got=%#v want=%#v", items, expected)
	}
}

func TestOccurrenceDamagedEvidenceRetainsT0511(t *testing.T) {
	for _, test := range []struct {
		name     string
		contents []byte
		kind     syntax.NodeKind
	}{
		{name: "in-tree error", contents: []byte(`@`), kind: syntax.Error},
		{name: "in-tree missing", contents: []byte(`fn broken(`), kind: syntax.Missing},
	} {
		t.Run(test.name, func(t *testing.T) {
			sources := source.NewFileSet()
			fileID, err := sources.Add("damaged.peb", test.contents)
			if err != nil {
				t.Fatal(err)
			}
			file, _ := sources.File(fileID)
			parserDiagnostics := diagnostic.NewDiagnosticSet()
			tree := syntax.Parse(file, parserDiagnostics)
			nodeID := treeNodeKind(t, tree, test.kind)
			for _, interpretation := range []string{"type", "value"} {
				program, store := testProgram(t)
				program.config = normalizeConfig(Config{})
				program.modules = map[module.ModuleID]module.Module{1: {ID: 1, Source: fileID, Tree: tree}}
				diagnostics := diagnostic.NewDiagnosticSet()
				session := NewSession(program, diagnostics, Config{})
				ref := symbol.SyntaxRef{Module: 1, Node: nodeID}
				if interpretation == "type" {
					session.Add(TypeOccurrence(ref, 0, session.Known(store.Builtins().Int), Origin{Syntax: ref, Role: test.name}))
				} else {
					session.Add(ValueOccurrence(ref, Origin{Syntax: ref, Role: test.name}))
				}
				if session.Solve().Successful() || !hasDiagnostic(diagnostics, CodeDamagedInput) || hasDiagnostic(diagnostics, CodeResourceLimit) {
					t.Fatalf("interpretation=%s parser=%+v inference=%+v", interpretation, parserDiagnostics.Items(), diagnostics.Items())
				}
			}
		})
	}

	t.Run("absent immutable resolution", func(t *testing.T) {
		program, preparation := prepareSource(t, []byte(`type Thing = int;`))
		if preparation.HasErrors() {
			t.Fatalf("prepare diagnostics: %+v", preparation.Items())
		}
		ref := nameWithoutResolution(t, program)
		for _, interpretation := range []string{"type", "value"} {
			diagnostics := diagnostic.NewDiagnosticSet()
			session := NewSession(program, diagnostics, Config{})
			if interpretation == "type" {
				session.Add(TypeOccurrence(ref, 0, session.Known(program.inputs.Types.Builtins().Int), Origin{Syntax: ref, Role: "absent resolution"}))
			} else {
				session.Add(ValueOccurrence(ref, Origin{Syntax: ref, Role: "absent resolution"}))
			}
			if session.Solve().Successful() || !hasDiagnostic(diagnostics, CodeDamagedInput) || hasDiagnostic(diagnostics, CodeResourceLimit) {
				t.Fatalf("interpretation=%s diagnostics=%+v", interpretation, diagnostics.Items())
			}
		}
	})
}
