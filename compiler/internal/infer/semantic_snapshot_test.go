package infer

import (
	"fmt"
	"reflect"
	"sort"
	"strings"
	"sync"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

const semanticSnapshotSource = `
type Base = struct { value int; };
type Box[T] = struct {
    value T;
    other int;
    fn map[U](self Box[T], value U) U => value;
};
type Wrapped[T] = Box[T];
fn preserve(value Allocator) Allocator => value;
fn use(value Base, box Wrapped[Base]) Base => value;
fn choose[U, V](left U, right V) U => left;
`

func TestSemanticSnapshotExactFinalizedSolutionsAndOwnership(t *testing.T) {
	program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
	if preparation.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", preparation.Items())
	}
	runtimeTypes, _ := program.RuntimeTypes()
	allocator, _ := program.inputs.Resolution.Runtime(symbol.RuntimeAllocator)

	session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	session.PublishSymbol(allocator, session.Known(runtimeTypes.Allocator))
	solution := session.Solve()
	diagnostics := diagnostic.NewDiagnosticSet()
	snapshot, ok := Snapshot(program, solution, diagnostics)
	if !ok || diagnostics.HasErrors() || !snapshot.Matches(solution) {
		t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, diagnostics.Items())
	}
	if snapshot.Resolution() != program.inputs.Resolution || snapshot.Types().Len() != solution.storeLength {
		t.Fatal("snapshot did not retain the exact resolution and captured type prefix")
	}
	if got, exists := solution.SymbolType(allocator); !exists || got != (TypeResult{State: TypeFinal, Type: runtimeTypes.Allocator}) {
		t.Fatalf("final symbol type=%+v exists=%v", got, exists)
	}

	failedSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	failedRef := firstProgramSyntaxRef(t, program)
	failedSession.PublishSyntax(failedRef, failedSession.Error(Origin{Syntax: failedRef}))
	failedSolution := failedSession.Solve()
	failedSnapshot, failedOK := Snapshot(program, failedSolution, diagnostic.NewDiagnosticSet())
	if failedSolution.Successful() || !failedOK || !failedSnapshot.Matches(failedSolution) {
		t.Fatal("a finalized unsuccessful solution was not accepted")
	}

	otherSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	otherSolution := otherSession.Solve()
	if snapshot.Matches(otherSolution) {
		t.Fatal("snapshot matched another session from the same program")
	}
	foreignProgram, _ := prepareSource(t, []byte(`fn other() void {}`))
	assertSnapshotFailure(t, foreignProgram, solution)
	assertSnapshotFailure(t, program, &Solution{})
	unfinalized := *solution
	unfinalized.finalized = false
	assertSnapshotFailure(t, program, &unfinalized)
}

func TestSemanticSnapshotRepeatedSolveRecoveryPreservesFirstResult(t *testing.T) {
	program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
	if preparation.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", preparation.Items())
	}
	runtimeTypes, _ := program.RuntimeTypes()
	allocator, _ := program.inputs.Resolution.Runtime(symbol.RuntimeAllocator)
	diagnostics := diagnostic.NewDiagnosticSet()
	session := NewSession(program, diagnostics, Config{})
	session.PublishSymbol(allocator, session.Known(runtimeTypes.Allocator))
	first := session.Solve()
	snapshot, ok := Snapshot(program, first, diagnostic.NewDiagnosticSet())
	if !ok || !snapshot.Matches(first) {
		t.Fatal("first finalized result was not accepted")
	}
	before, exists := first.SymbolType(allocator)
	second := session.Solve()
	if first == second || second.finalized || second.Successful() {
		t.Fatal("second Solve did not return a distinct rejected recovery")
	}
	assertSnapshotFailure(t, program, second)
	after, stillExists := first.SymbolType(allocator)
	if !snapshot.Matches(first) || !exists || !stillExists || before != after || after != (TypeResult{State: TypeFinal, Type: runtimeTypes.Allocator}) {
		t.Fatalf("first solution changed: before=%+v after=%+v", before, after)
	}
	if diagnostics.Len() != 1 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("repeated-Solve diagnostics=%+v", diagnostics.Items())
	}
}

func TestSemanticSnapshotInvalidSessionFinalizationIsNonPanicking(t *testing.T) {
	diagnostics := diagnostic.NewDiagnosticSet()
	solution := NewSession(nil, diagnostics, Config{}).Solve()
	if solution == nil || solution.programIdentity != nil || solution.storeLength != 0 || !solution.finalized || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("invalid solution=%+v diagnostics=%+v", solution, diagnostics.Items())
	}
	assertSnapshotFailure(t, nil, solution)
}

func TestSemanticSnapshotCopiesAndOrdersEverySemanticSlice(t *testing.T) {
	program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
	if preparation.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", preparation.Items())
	}
	solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
	snapshot, ok := Snapshot(program, solution, diagnostic.NewDiagnosticSet())
	if !ok {
		t.Fatal("snapshot failed")
	}

	templates := snapshot.Templates()
	for index, value := range templates {
		if value.ID != TemplateID(index+1) {
			t.Fatalf("template order=%v", templates)
		}
		if len(value.Children) != 0 {
			value.Children[0] = 0
			fresh, _ := snapshot.Template(value.ID)
			if fresh.Children[0] == 0 {
				t.Fatal("template children alias accessor storage")
			}
			break
		}
	}
	if len(templates) != 0 {
		templates[0].Kind = 0
		fresh, _ := snapshot.Template(1)
		if fresh.Kind == 0 {
			t.Fatal("template list aliases snapshot storage")
		}
	}

	declarations := snapshot.TypeDeclarations()
	assertIncreasingDeclarations(t, declarations)
	for _, declaration := range declarations {
		if len(declaration.Parameters) != 0 {
			declaration.Parameters[0] = 0
			fresh, _ := snapshot.TypeDeclaration(declaration.Symbol)
			if fresh.Parameters[0] == 0 {
				t.Fatal("declaration parameters alias snapshot storage")
			}
		}
		if len(declaration.Members) != 0 {
			declaration.Members[0] = MemberDescriptor{}
			fresh, _ := snapshot.TypeDeclaration(declaration.Symbol)
			if fresh.Members[0] == (MemberDescriptor{}) {
				t.Fatal("declaration members alias snapshot storage")
			}
		}
	}
	if len(declarations) != 0 {
		id := declarations[0].Symbol
		declarations[0].State = 0
		fresh, _ := snapshot.TypeDeclaration(id)
		if fresh.State == 0 {
			t.Fatal("declaration list aliases snapshot storage")
		}
	}

	signatures := snapshot.Signatures()
	assertIncreasingSignatures(t, signatures)
	for _, signature := range signatures {
		mutateFirstSymbol(signature.Parameters)
		mutateFirstSymbol(signature.TypeParams)
		if len(signature.Inputs) != 0 {
			signature.Inputs[0] = 0
		}
		fresh, _ := snapshot.Signature(signature.Symbol)
		if len(fresh.Parameters) != 0 && fresh.Parameters[0] == 0 || len(fresh.TypeParams) != 0 && fresh.TypeParams[0] == 0 || len(fresh.Inputs) != 0 && fresh.Inputs[0] == 0 {
			t.Fatal("signature slice aliases snapshot storage")
		}
	}
	if len(signatures) != 0 {
		id := signatures[0].Symbol
		signatures[0].State = 0
		fresh, _ := snapshot.Signature(id)
		if fresh.State == 0 {
			t.Fatal("signature list aliases snapshot storage")
		}
	}

	for _, owner := range snapshot.ownerIDs {
		parameters := snapshot.OwnerParameters(owner)
		if len(parameters) == 0 {
			continue
		}
		parameters[0] = 0
		if snapshot.OwnerParameters(owner)[0] == 0 {
			t.Fatal("owner parameters alias snapshot storage")
		}
	}
	for _, id := range snapshot.typeParamIDs {
		if value, exists := snapshot.TypeParameter(id); !exists || !snapshot.Types().Contains(value) {
			t.Fatalf("type parameter %d=%d exists=%v", id, value, exists)
		}
	}

	for index := range program.templates {
		if len(program.templates[index].Children) != 0 {
			id := program.templates[index].ID
			program.templates[index].Children[0] = 0
			fresh, _ := snapshot.Template(id)
			if fresh.Children[0] == 0 {
				t.Fatal("snapshot template retained Program backing storage")
			}
			break
		}
	}
	for owner, parameters := range program.owners {
		if len(parameters) != 0 {
			program.owners[owner][0] = 0
			if snapshot.OwnerParameters(owner)[0] == 0 {
				t.Fatal("snapshot owner retained Program backing storage")
			}
			break
		}
	}
}

func TestSemanticSnapshotRejectsStaleTypesAndDamagedFinalTables(t *testing.T) {
	t.Run("stale store", func(t *testing.T) {
		program, _ := prepareSource(t, []byte(semanticSnapshotSource))
		solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
		before := program.inputs.Types.Len()
		if _, err := program.internType(types.OptionalKey(program.inputs.Types.Builtins().Bool)); err != nil {
			t.Fatal(err)
		}
		assertSnapshotFailure(t, program, solution)
		if program.inputs.Types.Len() != before+1 {
			t.Fatal("failed snapshot changed the store")
		}
	})

	for _, test := range []struct {
		name   string
		damage func(*Solution)
	}{
		{name: "invalid final type", damage: func(solution *Solution) {
			id := solution.manifest.symbols[0]
			solution.symbols[id] = TypeResult{State: TypeFinal, Type: types.TypeID(solution.storeLength + 1)}
		}},
		{name: "error payload", damage: func(solution *Solution) {
			id := solution.manifest.symbols[0]
			solution.symbols[id] = TypeResult{State: TypeError, Type: 1}
		}},
		{name: "missing symbol table entry", damage: func(solution *Solution) {
			delete(solution.symbols, solution.manifest.symbols[0])
		}},
		{name: "foreign slot", damage: func(solution *Solution) {
			id := solution.manifest.slots[0]
			value := solution.slots[id]
			delete(solution.slots, id)
			foreign := SlotID{owner: &sessionToken{}, ordinal: id.ordinal}
			solution.slots[foreign] = value
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, _ := prepareSource(t, []byte(semanticSnapshotSource))
			runtimeTypes, _ := program.RuntimeTypes()
			allocator, _ := program.inputs.Resolution.Runtime(symbol.RuntimeAllocator)
			session := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
			session.PublishSymbol(allocator, session.Known(runtimeTypes.Allocator))
			session.PublishSlot(session.Known(runtimeTypes.Context))
			solution := session.Solve()
			test.damage(solution)
			assertSnapshotFailure(t, program, solution)
		})
	}
}

func TestSemanticSnapshotRejectsDamagedProgramRecordsAtomically(t *testing.T) {
	for _, test := range []struct {
		name   string
		damage func(*Program)
	}{
		{name: "template tag", damage: func(program *Program) { program.templates[0].Kind = 255 }},
		{name: "template child", damage: func(program *Program) {
			for index := range program.templates {
				if len(program.templates[index].Children) != 0 {
					program.templates[index].Children[0] = TemplateID(len(program.templates) + 1)
					return
				}
			}
		}},
		{name: "descriptor", damage: func(program *Program) {
			for id, declaration := range program.declarations {
				if len(declaration.Members) != 0 {
					declaration.Members[0].Type = TemplateID(len(program.templates) + 1)
					program.declarations[id] = declaration
					return
				}
			}
		}},
		{name: "signature", damage: func(program *Program) {
			for id, signature := range program.signatures {
				signature.Result = TemplateID(len(program.templates) + 1)
				program.signatures[id] = signature
				return
			}
		}},
		{name: "owner", damage: func(program *Program) {
			for owner, parameters := range program.owners {
				if len(parameters) != 0 {
					program.owners[owner] = append(parameters, parameters[0])
					return
				}
			}
		}},
		{name: "runtime", damage: func(program *Program) { program.runtimeTypes.Context = types.TypeID(program.inputs.Types.Len() + 1) }},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, _ := prepareSource(t, []byte(semanticSnapshotSource))
			solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
			storeLength := program.inputs.Types.Len()
			test.damage(program)
			assertSnapshotFailure(t, program, solution)
			if program.inputs.Types.Len() != storeLength {
				t.Fatal("failed snapshot changed the type store")
			}
		})
	}
}

func TestSemanticSnapshotRejectsExactResolverJoinDamage(t *testing.T) {
	for _, test := range []struct {
		name   string
		damage func(*testing.T, *Program)
	}{
		{name: "member authored order", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "Box", symbol.SymbolType)
			value := program.declarations[id]
			value.Members[0], value.Members[1] = value.Members[1], value.Members[0]
			program.declarations[id] = value
		}},
		{name: "signature parameter order", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "choose", symbol.SymbolFunction)
			value := program.signatures[id]
			value.Parameters[0], value.Parameters[1] = value.Parameters[1], value.Parameters[0]
			program.signatures[id] = value
		}},
		{name: "signature type parameter order", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "choose", symbol.SymbolFunction)
			value := program.signatures[id]
			value.TypeParams[0], value.TypeParams[1] = value.TypeParams[1], value.TypeParams[0]
			program.signatures[id] = value
		}},
		{name: "missing owner binding", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "Box", symbol.SymbolType)
			program.owners[id] = nil
		}},
		{name: "extra owner binding", damage: func(t *testing.T, program *Program) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			choose := semanticSymbol(t, program, "choose", symbol.SymbolFunction)
			program.owners[box] = append(program.owners[box], program.owners[choose][0])
		}},
		{name: "orphaned rigid binding", damage: func(t *testing.T, program *Program) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			delete(program.typeParams, program.owners[box][0])
		}},
		{name: "duplicate owner binding", damage: func(t *testing.T, program *Program) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			program.owners[box] = append(program.owners[box], program.owners[box][0])
		}},
		{name: "recovery declaration form", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "Base", symbol.SymbolType)
			value := program.declarations[id]
			value.State = DeclarationError
			value.Form = TypeDeclarationForm(255)
			program.declarations[id] = value
		}},
		{name: "recovery declaration nominal", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "Base", symbol.SymbolType)
			value := program.declarations[id]
			value.State = DeclarationError
			value.Nominal = NominalKind(255)
			program.declarations[id] = value
		}},
		{name: "recovery signature convention", damage: func(t *testing.T, program *Program) {
			id := semanticSymbol(t, program, "choose", symbol.SymbolFunction)
			value := program.signatures[id]
			value.State = DeclarationError
			value.Convention = types.CallingConvention(255)
			program.signatures[id] = value
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
			if preparation.HasErrors() {
				t.Fatalf("preparation diagnostics: %+v", preparation.Items())
			}
			solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
			test.damage(t, program)
			assertSnapshotFailure(t, program, solution)
		})
	}
}

func TestSemanticSnapshotRejectsCompleteFinalTableDamage(t *testing.T) {
	for _, test := range []struct {
		name   string
		damage func(*testing.T, *Program, *Solution)
	}{
		{name: "requirement parameter owner", damage: func(t *testing.T, program *Program, solution *Solution) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			choose := semanticSymbol(t, program, "choose", symbol.SymbolFunction)
			parameter := program.owners[box][0]
			solution.requirements[choose] = []Requirement{semanticRequirement(program, choose, parameter)}
			solution.manifest.requirements = []requirementTableManifest{{owner: choose, count: 1}}
		}},
		{name: "requirement stable order", damage: func(t *testing.T, program *Program, solution *Solution) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			parameter := program.owners[box][0]
			first := semanticRequirement(program, box, parameter)
			second := first
			second.Kind = RequirementOrdered
			solution.requirements[box] = []Requirement{second, first}
			solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 2}}
		}},
		{name: "requirement recovery tag", damage: func(t *testing.T, program *Program, solution *Solution) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			parameter := program.owners[box][0]
			value := semanticRequirement(program, box, parameter)
			value.Kind = RequirementKind(255)
			solution.requirements[box] = []Requirement{value}
			solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 1}}
		}},
		{name: "instantiation argument count", damage: func(t *testing.T, program *Program, solution *Solution) {
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			site := firstProgramSyntaxRef(t, program)
			solution.instantiations[site] = Instantiation{Site: site, Generic: box}
			solution.manifest.instantiations = []symbol.SyntaxRef{site}
		}},
		{name: "method local argument count", damage: func(t *testing.T, program *Program, solution *Solution) {
			method := semanticSymbol(t, program, "map", symbol.SymbolMethod)
			site := firstProgramSyntaxRef(t, program)
			solution.methods[site] = MethodSelection{Site: site, Method: method}
			solution.manifest.methods = []symbol.SyntaxRef{site}
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
			if preparation.HasErrors() {
				t.Fatalf("preparation diagnostics: %+v", preparation.Items())
			}
			solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
			test.damage(t, program, solution)
			assertSnapshotFailure(t, program, solution)
		})
	}
}

func TestSemanticSnapshotAcceptsCompleteFinalTablesAndRolelessValueSyntax(t *testing.T) {
	program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
	if preparation.HasErrors() {
		t.Fatalf("preparation diagnostics: %+v", preparation.Items())
	}
	solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
	box := semanticSymbol(t, program, "Box", symbol.SymbolType)
	parameter := program.owners[box][0]
	requirement := semanticRequirement(program, box, parameter)
	solution.requirements[box] = []Requirement{requirement}
	solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 1}}

	method := semanticSymbol(t, program, "map", symbol.SymbolMethod)
	refs := semanticSyntaxRefs(t, program, 2)
	solution.instantiations[refs[0]] = Instantiation{
		Site: refs[0], Generic: box,
		Arguments: []TypeResult{{State: TypeFinal, Type: program.inputs.Types.Builtins().Int}},
	}
	solution.manifest.instantiations = []symbol.SyntaxRef{refs[0]}
	solution.methods[refs[1]] = MethodSelection{
		Site: refs[1], Method: method,
		Arguments: []TypeResult{{State: TypeFinal, Type: program.inputs.Types.Builtins().Char}},
	}
	solution.manifest.methods = []symbol.SyntaxRef{refs[1]}

	diagnostics := diagnostic.NewDiagnosticSet()
	snapshot, ok := Snapshot(program, solution, diagnostics)
	if !ok || snapshot == nil || diagnostics.HasErrors() || !snapshot.Matches(solution) {
		t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, diagnostics.Items())
	}
}

func TestSemanticSnapshotRequirementLiteralPayloads(t *testing.T) {
	t.Run("valid canonical integer and rational", func(t *testing.T) {
		program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
		if preparation.HasErrors() {
			t.Fatalf("preparation diagnostics: %+v", preparation.Items())
		}
		solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
		box := semanticSymbol(t, program, "Box", symbol.SymbolType)
		parameter := program.owners[box][0]
		integer := semanticLiteralRequirement(program, box, parameter, ExactInteger, "-17", "")
		rational := semanticLiteralRequirement(program, box, parameter, ExactFloat, "-17", "3")
		solution.requirements[box] = []Requirement{integer, rational}
		solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 2}}
		diagnostics := diagnostic.NewDiagnosticSet()
		snapshot, ok := Snapshot(program, solution, diagnostics)
		if !ok || snapshot == nil || diagnostics.HasErrors() || !snapshot.Matches(solution) {
			t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, diagnostics.Items())
		}
	})

	for _, test := range []struct {
		name        string
		literalKind ExactLiteralKind
		numerator   string
		denominator string
	}{
		{name: "integer empty", literalKind: ExactInteger, numerator: ""},
		{name: "integer plus", literalKind: ExactInteger, numerator: "+1"},
		{name: "integer leading zero", literalKind: ExactInteger, numerator: "01"},
		{name: "integer negative zero", literalKind: ExactInteger, numerator: "-0"},
		{name: "integer negative leading zero", literalKind: ExactInteger, numerator: "-01"},
		{name: "integer whitespace", literalKind: ExactInteger, numerator: " 1"},
		{name: "integer nondigit", literalKind: ExactInteger, numerator: "1x"},
		{name: "integer denominator", literalKind: ExactInteger, numerator: "1", denominator: "2"},
		{name: "float malformed numerator", literalKind: ExactFloat, numerator: "--1", denominator: "2"},
		{name: "float empty denominator", literalKind: ExactFloat, numerator: "1", denominator: ""},
		{name: "float zero denominator", literalKind: ExactFloat, numerator: "1", denominator: "0"},
		{name: "float negative denominator", literalKind: ExactFloat, numerator: "1", denominator: "-2"},
		{name: "float plus denominator", literalKind: ExactFloat, numerator: "1", denominator: "+2"},
		{name: "float leading-zero denominator", literalKind: ExactFloat, numerator: "1", denominator: "02"},
		{name: "float whitespace denominator", literalKind: ExactFloat, numerator: "1", denominator: "2 "},
		{name: "float nondigit denominator", literalKind: ExactFloat, numerator: "1", denominator: "2x"},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
			if preparation.HasErrors() {
				t.Fatalf("preparation diagnostics: %+v", preparation.Items())
			}
			solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			parameter := program.owners[box][0]
			value := semanticLiteralRequirement(program, box, parameter, test.literalKind, test.numerator, test.denominator)
			solution.requirements[box] = []Requirement{value}
			solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 1}}
			storeLength := program.inputs.Types.Len()
			assertSnapshotFailure(t, program, solution)
			if program.inputs.Types.Len() != storeLength || len(solution.Requirements(box)) != 1 {
				t.Fatal("literal validation failure mutated the store or finalized solution")
			}
		})
	}
}

func TestSemanticSnapshotRejectsDuplicateSemanticRequirements(t *testing.T) {
	for _, test := range []struct {
		name    string
		literal bool
		second  func(Requirement) Requirement
	}{
		{name: "exact duplicate", literal: true, second: func(value Requirement) Requirement { return value }},
		{name: "same semantic key different origin", second: func(value Requirement) Requirement {
			value.Origin.Role = "z duplicate origin"
			return value
		}},
	} {
		t.Run(test.name, func(t *testing.T) {
			program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
			if preparation.HasErrors() {
				t.Fatalf("preparation diagnostics: %+v", preparation.Items())
			}
			solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
			box := semanticSymbol(t, program, "Box", symbol.SymbolType)
			parameter := program.owners[box][0]
			first := semanticRequirement(program, box, parameter)
			if test.literal {
				first = semanticLiteralRequirement(program, box, parameter, ExactFloat, "5", "7")
			}
			first.Origin.Role = "a original origin"
			second := test.second(first)
			solution.requirements[box] = []Requirement{first, second}
			solution.manifest.requirements = []requirementTableManifest{{owner: box, count: 2}}
			storeLength := program.inputs.Types.Len()
			assertSnapshotFailure(t, program, solution)
			if program.inputs.Types.Len() != storeLength || len(solution.Requirements(box)) != 2 {
				t.Fatal("duplicate validation failure mutated the store or finalized solution")
			}
		})
	}
}

func TestSemanticSnapshotDamagedValidationIsDeterministic(t *testing.T) {
	var expected string
	for iteration := 0; iteration < 32; iteration++ {
		program, preparation := prepareSource(t, []byte(semanticSnapshotSource))
		if preparation.HasErrors() {
			t.Fatalf("preparation diagnostics: %+v", preparation.Items())
		}
		solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
		ids := sortedMapSymbolIDs(program.declarations)
		for index := len(ids) - 1; index >= 0; index-- {
			value := program.declarations[ids[index]]
			value.State = 0
			program.declarations[ids[index]] = value
		}
		diagnostics := diagnostic.NewDiagnosticSet()
		if snapshot, ok := Snapshot(program, solution, diagnostics); ok || snapshot != nil {
			t.Fatal("damaged snapshot unexpectedly succeeded")
		}
		got := fmt.Sprintf("%+v", diagnostics.Items())
		if iteration == 0 {
			expected = got
		} else if got != expected {
			t.Fatalf("diagnostics changed on iteration %d:\nwant %s\n got %s", iteration, expected, got)
		}
	}
}

func TestSemanticSnapshotLimitsAreBoundedAndAtomic(t *testing.T) {
	program, _ := prepareSource(t, []byte(semanticSnapshotSource))
	solution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
	program.config.MaxDecompositionSteps = 1
	program.config.MaxDiagnostics = 1
	diagnostics := diagnostic.NewDiagnosticSet()
	snapshot, ok := Snapshot(program, solution, diagnostics)
	if ok || snapshot != nil || diagnostics.Len() != 1 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, diagnostics.Items())
	}
	if !solution.finalized || program.inputs.Types.Len() != solution.storeLength {
		t.Fatal("limit failure changed the program, solution, or store")
	}
}

func TestSemanticSnapshotSequentialSessionsIndependentAndConcurrentReads(t *testing.T) {
	program, _ := prepareSource(t, []byte(semanticSnapshotSource))
	firstSolution := NewSession(program, diagnostic.NewDiagnosticSet(), Config{}).Solve()
	first, ok := Snapshot(program, firstSolution, diagnostic.NewDiagnosticSet())
	if !ok {
		t.Fatal("first snapshot failed")
	}

	newType, err := program.internType(types.OptionalKey(program.inputs.Types.Builtins().Char))
	if err != nil {
		t.Fatal(err)
	}
	secondSession := NewSession(program, diagnostic.NewDiagnosticSet(), Config{})
	slot := secondSession.PublishSlot(secondSession.Known(newType))
	secondSolution := secondSession.Solve()
	second, ok := Snapshot(program, secondSolution, diagnostic.NewDiagnosticSet())
	if !ok || second.Types().Len() != first.Types().Len()+1 || first.Types().Contains(newType) || !second.Types().Contains(newType) {
		t.Fatalf("independent lengths first=%d second=%d type=%d", first.Types().Len(), second.Types().Len(), newType)
	}
	if _, exists := secondSolution.Slot(slot); !exists || first.Matches(secondSolution) || second.Matches(firstSolution) {
		t.Fatal("session identities or later store use crossed snapshot ownership")
	}

	var wait sync.WaitGroup
	for range 16 {
		wait.Add(1)
		go func() {
			defer wait.Done()
			for range 100 {
				_ = second.Types().Builtins()
				_ = second.TypeDeclarations()
				_ = second.Signatures()
				_ = second.Templates()
				for _, owner := range second.ownerIDs {
					_ = second.OwnerParameters(owner)
				}
				if !second.Matches(secondSolution) {
					t.Error("concurrent match failed")
					return
				}
			}
		}()
	}
	wait.Wait()
}

func TestSemanticSnapshotRetainsNoASTOrMutableOwner(t *testing.T) {
	forbidden := []string{"Program", "Session", "Store", "Graph", "Module", "FileSet", "syntax.Tree", "syntax.Node", "Constraint", "Term", "InferID", "reporter"}
	typeOf := reflect.TypeOf(SemanticSnapshot{})
	for index := 0; index < typeOf.NumField(); index++ {
		field := typeOf.Field(index)
		name := field.Type.String()
		for _, fragment := range forbidden {
			if strings.Contains(name, fragment) {
				t.Fatalf("SemanticSnapshot.%s retains forbidden %s", field.Name, name)
			}
		}
	}
}

func assertSnapshotFailure(t *testing.T, program *Program, solution *Solution) {
	t.Helper()
	diagnostics := diagnostic.NewDiagnosticSet()
	snapshot, ok := Snapshot(program, solution, diagnostics)
	if ok || snapshot != nil || diagnostics.Len() != 1 || !hasDiagnostic(diagnostics, CodeResourceLimit) {
		t.Fatalf("snapshot=(%v,%v) diagnostics=%+v", snapshot, ok, diagnostics.Items())
	}
}

func firstProgramSyntaxRef(t *testing.T, program *Program) symbol.SyntaxRef {
	t.Helper()
	for _, item := range program.modules {
		if item.Tree != nil {
			return symbol.SyntaxRef{Module: item.ID, Node: item.Tree.Root()}
		}
	}
	t.Fatal("program has no syntax")
	return symbol.SyntaxRef{}
}

func assertIncreasingDeclarations(t *testing.T, values []TypeDeclaration) {
	t.Helper()
	for index := 1; index < len(values); index++ {
		if values[index-1].Symbol >= values[index].Symbol {
			t.Fatalf("declarations are not ordered: %+v", values)
		}
	}
}

func assertIncreasingSignatures(t *testing.T, values []Signature) {
	t.Helper()
	for index := 1; index < len(values); index++ {
		if values[index-1].Symbol >= values[index].Symbol {
			t.Fatalf("signatures are not ordered: %+v", values)
		}
	}
}

func mutateFirstSymbol(values []symbol.SymbolID) {
	if len(values) != 0 {
		values[0] = 0
	}
}

func semanticSymbol(t *testing.T, program *Program, name string, kind symbol.SymbolKind) symbol.SymbolID {
	t.Helper()
	for _, value := range program.inputs.Resolution.Symbols.All() {
		if value.Name == name && value.Kind == kind && !value.Error {
			return value.ID
		}
	}
	t.Fatalf("missing %s symbol %q", kind, name)
	return 0
}

func semanticRequirement(program *Program, owner, parameter symbol.SymbolID) Requirement {
	ownerSymbol, _ := program.inputs.Resolution.Symbols.Symbol(owner)
	return Requirement{
		Owner: owner, Parameter: parameter, Kind: RequirementNumeric,
		Subject: program.typeParams[parameter],
		Origin:  Origin{Syntax: ownerSymbol.Declaration, GenericOwner: owner, Role: "test requirement"},
	}
}

func semanticLiteralRequirement(program *Program, owner, parameter symbol.SymbolID, kind ExactLiteralKind, numerator, denominator string) Requirement {
	value := semanticRequirement(program, owner, parameter)
	value.Kind = RequirementLiteralFits
	value.LiteralKind = kind
	value.Numerator = numerator
	value.Denominator = denominator
	return value
}

func semanticSyntaxRefs(t *testing.T, program *Program, count int) []symbol.SyntaxRef {
	t.Helper()
	moduleIDs := make([]int, 0, len(program.modules))
	for id := range program.modules {
		moduleIDs = append(moduleIDs, int(id))
	}
	sort.Ints(moduleIDs)
	result := make([]symbol.SyntaxRef, 0, count)
	for _, rawID := range moduleIDs {
		item := program.modules[module.ModuleID(rawID)]
		for node := syntax.NodeID(1); item.Tree != nil && node <= item.Tree.Root() && len(result) < count; node++ {
			ref := symbol.SyntaxRef{Module: item.ID, Node: node}
			if _, hasReference := program.inputs.Resolution.Reference(ref); hasReference {
				continue
			}
			if _, hasQualifier := program.inputs.Resolution.Qualifier(ref); hasQualifier {
				continue
			}
			if _, hasBracket := program.inputs.Resolution.Bracket(ref); hasBracket {
				continue
			}
			if len(program.inputs.Resolution.Captures(ref)) != 0 {
				continue
			}
			result = append(result, ref)
		}
	}
	if len(result) != count {
		t.Fatalf("found %d roleless syntax refs, need %d", len(result), count)
	}
	return result
}
