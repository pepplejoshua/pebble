package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// crossModuleGenericHelperSource and crossModuleGenericRootSource are the 07.6c
// fixture: one imported module (helper.peb) owns a generic function, and the
// root module imports it and uses the same generic at repeated call sites plus
// bare generic function values. Every (generic symbol, concrete TypeArgs,
// calling convention) key is requested from multiple consumer sites in the root
// module: identity[i32] comes from two call sites and a bare value, and
// identity[char] from a call site and a bare value. Block bodies are required
// (expression bodies lower to an empty Block -- the pre-existing gap recorded
// under 07.3f) so the built specialization bodies contain real content.
const crossModuleGenericHelperSource = `fn identity[T](value T) T { return value; }
`

const crossModuleGenericRootSource = `import "./helper";

fn callBoth() void {
    let a i32 = helper::identity(1);
    let b i32 = helper::identity(2);
    let c char = helper::identity('x');
    let d fn(i32) i32 = helper::identity[i32];
    let e fn(char) char = helper::identity[char];
    print a;
    print b;
    print c;
    print d(1);
    print e('y');
}

fn main() void {
    callBoth();
}
`

// crossModuleGenericFixture holds the checked two-module program plus the
// identities the cross-module sharing assertions are written against.
type crossModuleGenericFixture struct {
	inputs       Inputs
	result       *Result
	unit         *tir.Unit
	generic      symbol.SymbolID
	rootModule   module.ModuleID
	helperModule module.ModuleID
}

// buildCrossModuleGenericFixture runs the fixture through the normal public
// Check/factInputs pipeline and pins the imported generic symbol and both
// module IDs.
func buildCrossModuleGenericFixture(t *testing.T) *crossModuleGenericFixture {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{
		"main.peb":   []byte(crossModuleGenericRootSource),
		"helper.peb": []byte(crossModuleGenericHelperSource),
	})
	result := Check(inputs, diagnostics, Config{})
	if !result.Successful() {
		t.Fatalf("cross-module generic fixture rejected: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("successful cross-module generic result has nil IR")
	}
	var rootModule, helperModule module.ModuleID
	for _, m := range unit.Modules() {
		switch m.Key.Path {
		case "main.peb":
			rootModule = m.ID
		case "helper.peb":
			helperModule = m.ID
		}
	}
	if rootModule == 0 || helperModule == 0 {
		t.Fatalf("root/helper module IDs missing from %+v", unit.Modules())
	}
	var generic symbol.SymbolID
	for _, candidate := range inputs.Resolution.Symbols.All() {
		if candidate.Name == "identity" && candidate.Kind == symbol.SymbolFunction {
			generic = candidate.ID
			break
		}
	}
	if generic == 0 {
		t.Fatal("imported generic function symbol not resolved")
	}
	return &crossModuleGenericFixture{
		inputs: inputs, result: result, unit: unit,
		generic: generic, rootModule: rootModule, helperModule: helperModule,
	}
}

// specializationDeclarations returns the generic's FunctionDeclaration nodes
// that carry concrete TypeArgs (the real specializations), excluding the
// symbolic declaration whose TypeArgs stay empty.
func (fx *crossModuleGenericFixture) specializationDeclarations() []tir.Node {
	var out []tir.Node
	for _, node := range fx.unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == fx.generic && len(node.TypeArgs) > 0 {
			out = append(out, node)
		}
	}
	return out
}

// equalTypeArgs reports whether two concrete type-argument lists are identical.
func equalTypeArgs(a, b []types.TypeID) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// conventionForValue resolves the calling convention a bare generic function
// value carries: GenericFunctionValue nodes do not store Convention directly,
// so it is taken from the unique specialization declaration matching the
// value's (symbol, TypeArgs) triple.
func (fx *crossModuleGenericFixture) conventionForValue(node tir.Node) (types.CallingConvention, bool) {
	found := false
	var convention types.CallingConvention
	for _, candidate := range fx.unit.Nodes() {
		if candidate.Kind != tir.FunctionDeclaration || candidate.Symbol != node.Symbol || len(candidate.TypeArgs) == 0 || !equalTypeArgs(candidate.TypeArgs, node.TypeArgs) {
			continue
		}
		if found {
			return 0, false
		}
		found = true
		convention = candidate.Convention
	}
	return convention, found
}

// consumerSpecializationKey derives the exact specialization key one consumer
// reference (a generic call site or a bare generic function value) carries.
func (fx *crossModuleGenericFixture) consumerSpecializationKey(t *testing.T, node tir.Node) (specializationKey, bool) {
	t.Helper()
	switch node.Kind {
	case tir.DirectCall:
		if len(node.TypeArgs) == 0 {
			return specializationKey{}, false
		}
		return newSpecializationKey(node.Symbol, node.TypeArgs, node.Convention), true
	case tir.GenericFunctionValue:
		convention, ok := fx.conventionForValue(node)
		if !ok {
			return specializationKey{}, false
		}
		return newSpecializationKey(node.Symbol, node.TypeArgs, convention), true
	}
	return specializationKey{}, false
}

// consumerReferences returns every root-module node that resolves to the
// imported generic with concrete type arguments.
func (fx *crossModuleGenericFixture) consumerReferences() []tir.Node {
	var out []tir.Node
	for _, node := range fx.unit.Nodes() {
		if node.Kind != tir.DirectCall && node.Kind != tir.GenericFunctionValue {
			continue
		}
		if node.Symbol == fx.generic && len(node.TypeArgs) > 0 {
			out = append(out, node)
		}
	}
	return out
}

// requireSpecializationBody asserts a specialization FunctionDeclaration really
// was built: its FunctionID must map to a FunctionDecl whose node is a Block
// containing one Return whose single value carries the substituted parameter
// type.
func requireSpecializationBody(t *testing.T, fx *crossModuleGenericFixture, declaration tir.Node) {
	t.Helper()
	var body tir.FunctionDecl
	for _, candidate := range fx.unit.FunctionDeclarations() {
		if candidate.FunctionID == declaration.Function {
			body = candidate
			break
		}
	}
	if body.Node == 0 {
		t.Fatalf("specialization declaration Function=%d has no built body", declaration.Function)
	}
	block := fx.unit.Nodes()[body.Node-1]
	if block.Kind != tir.Block || len(block.Children) != 1 {
		t.Fatalf("specialization body = %+v, want a one-return Block", block)
	}
	returnNode := fx.unit.Nodes()[block.Children[0]-1]
	if returnNode.Kind != tir.Return || len(returnNode.Children) != 1 {
		t.Fatalf("specialization body statement = %+v, want a single-value Return", returnNode)
	}
	value := fx.unit.Nodes()[returnNode.Children[0]-1]
	if value.Type != declaration.Parameters[0].Type {
		t.Fatalf("specialized return value type = %v, want the substituted parameter type %v", value.Type, declaration.Parameters[0].Type)
	}
}

func TestCrossModuleGenericTwoModulesAndImportedOwnership(t *testing.T) {
	fx := buildCrossModuleGenericFixture(t)

	modules := fx.unit.Modules()
	if len(modules) != 2 {
		t.Fatalf("modules = %d, want exactly two", len(modules))
	}
	var root, helper *tir.ModuleDecl
	for i := range modules {
		switch modules[i].Key.Path {
		case "main.peb":
			root = &modules[i]
		case "helper.peb":
			helper = &modules[i]
		}
	}
	if root == nil || helper == nil {
		t.Fatalf("root/helper module declarations missing: %+v", modules)
	}
	if root.ID != fx.rootModule || helper.ID != fx.helperModule {
		t.Fatalf("module IDs disagree: root=%d helper=%d", root.ID, helper.ID)
	}
	if len(helper.Imports) != 0 {
		t.Fatalf("helper module imports = %d, want 0", len(helper.Imports))
	}
	if len(root.Imports) != 1 || root.Imports[0].Target != fx.helperModule {
		t.Fatalf("root imports = %+v, want a single import of helper module %d", root.Imports, fx.helperModule)
	}

	// Exactly one function symbol named "identity" exists and it is owned by the
	// declaring helper module: the root call sites must not resolve to a local
	// duplicate.
	identityCount := 0
	for _, candidate := range fx.inputs.Resolution.Symbols.All() {
		if candidate.Name == "identity" && candidate.Kind == symbol.SymbolFunction {
			identityCount++
			if candidate.ID != fx.generic {
				t.Fatalf("duplicate identity symbol %d resolves to module %d, want the single imported symbol %d", candidate.ID, candidate.Module, fx.generic)
			}
			if candidate.Module != fx.helperModule {
				t.Fatalf("imported generic symbol module = %d, want declaring helper module %d", candidate.Module, fx.helperModule)
			}
		}
	}
	if identityCount != 1 {
		t.Fatalf("function symbols named %q = %d, want exactly one imported generic", "identity", identityCount)
	}
	declaredByHelper := false
	for _, id := range helper.Declarations {
		if id == fx.generic {
			declaredByHelper = true
		}
	}
	if !declaredByHelper {
		t.Fatal("helper module does not declare the generic function")
	}
	for _, id := range root.Declarations {
		if id == fx.generic {
			t.Fatalf("root module declares symbol %d: the generic is duplicated locally instead of imported", id)
		}
	}
}

func TestCrossModuleGenericRootCallSitesResolveToImportedSymbol(t *testing.T) {
	fx := buildCrossModuleGenericFixture(t)

	references := fx.consumerReferences()
	if len(references) != 5 {
		t.Fatalf("consumer references to the imported generic = %d, want 5 (two i32 calls, one char call, two bare values)", len(references))
	}
	for _, node := range references {
		if node.Symbol != fx.generic {
			t.Fatalf("consumer reference resolves to symbol %d, want imported generic %d", node.Symbol, fx.generic)
		}
	}

	// Every solved instantiation the pipeline hands to buildSpecializations
	// targets the imported generic symbol: no local copy is ever instantiated.
	instantiations := fx.result.Solution().Instantiations()
	if len(instantiations) != 5 {
		t.Fatalf("solved instantiations = %d, want 5 consumer sites", len(instantiations))
	}
	for _, instantiation := range instantiations {
		if instantiation.Generic != fx.generic {
			t.Fatalf("solved instantiation at %v targets generic %d, want imported generic %d", instantiation.Site, instantiation.Generic, fx.generic)
		}
	}
}

func TestCrossModuleGenericOneSharedSpecializationPerKey(t *testing.T) {
	fx := buildCrossModuleGenericFixture(t)

	symbolicCount := 0
	for _, node := range fx.unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == fx.generic && len(node.TypeArgs) == 0 {
			symbolicCount++
		}
	}
	if symbolicCount != 1 {
		t.Fatalf("symbolic declarations for the generic = %d, want exactly one", symbolicCount)
	}

	byKey := make(map[specializationKey]tir.Node)
	for _, node := range fx.specializationDeclarations() {
		key := newSpecializationKey(node.Symbol, node.TypeArgs, node.Convention)
		if previous, duplicate := byKey[key]; duplicate {
			t.Fatalf("duplicate specialization declaration nodes %d and %d share key %+v", previous.Function, node.Function, key)
		}
		byKey[key] = node
		if node.Function == 0 {
			t.Fatalf("specialization declaration %+v has no function identity", node)
		}
		if len(node.Parameters) != 1 || node.Parameters[0].Type != node.ResultType || node.ResultType != node.TypeArgs[0] {
			t.Fatalf("specialization declaration = %+v, want the substituted identity signature T->T", node)
		}
		requireSpecializationBody(t, fx, node)
	}

	// Exactly the two distinct keys the fixture's call sites request, and each
	// resolves to a shared specialization with the exact concrete type argument.
	i32 := fx.inputs.Types.Builtins().I32
	char := fx.inputs.Types.Builtins().Char
	if len(byKey) != 2 {
		t.Fatalf("distinct specialization keys = %d, want 2 (i32 and char)", len(byKey))
	}
	if declaration, ok := byKey[newSpecializationKey(fx.generic, []types.TypeID{i32}, types.Pebble)]; !ok {
		t.Fatal("missing shared specialization for the i32 key")
	} else if declaration.TypeArgs[0] != i32 {
		t.Fatalf("i32 specialization TypeArgs = %v, want [%d]", declaration.TypeArgs, i32)
	}
	if declaration, ok := byKey[newSpecializationKey(fx.generic, []types.TypeID{char}, types.Pebble)]; !ok {
		t.Fatal("missing shared specialization for the char key")
	} else if declaration.TypeArgs[0] != char {
		t.Fatalf("char specialization TypeArgs = %v, want [%d]", declaration.TypeArgs, char)
	}

	// The number of specialization declarations equals the number of distinct
	// keys: repeated same-key requests added no duplicate declarations.
	if len(fx.specializationDeclarations()) != len(byKey) {
		t.Fatalf("specialization declarations = %d, distinct keys = %d", len(fx.specializationDeclarations()), len(byKey))
	}
}

func TestCrossModuleGenericConsumersShareOneSpecialization(t *testing.T) {
	fx := buildCrossModuleGenericFixture(t)

	// One specialization declaration per key, with a stable function identity.
	specByKey := make(map[specializationKey]tir.Node)
	for _, node := range fx.specializationDeclarations() {
		key := newSpecializationKey(node.Symbol, node.TypeArgs, node.Convention)
		if _, duplicate := specByKey[key]; duplicate {
			t.Fatalf("duplicate specialization declaration for key %+v", key)
		}
		specByKey[key] = node
	}

	// Every consumer reference resolves to the one specialization matching its
	// (symbol, TypeArgs, convention) key, and all consumers of the same key
	// agree on that single specialization's function identity.
	keyedFunctions := make(map[specializationKey]tir.FunctionID)
	references := fx.consumerReferences()
	if len(references) != 5 {
		t.Fatalf("consumer references = %d, want 5", len(references))
	}
	for _, node := range references {
		key, ok := fx.consumerSpecializationKey(t, node)
		if !ok {
			t.Fatalf("consumer reference %+v does not resolve to a specialization key", node)
		}
		declaration, ok := specByKey[key]
		if !ok {
			t.Fatalf("consumer reference carries key %+v with no matching specialization declaration", key)
		}
		if previous, seen := keyedFunctions[key]; seen {
			if previous != declaration.Function {
				t.Fatalf("consumers of key %+v disagree on function identity: %d vs %d", key, previous, declaration.Function)
			}
		} else {
			keyedFunctions[key] = declaration.Function
		}
	}
	if len(keyedFunctions) != 2 {
		t.Fatalf("consumer keys = %d, want 2 (i32 and char)", len(keyedFunctions))
	}
}

func TestCrossModuleGenericRepeatedKeysNoDuplicateTableOrDeclarations(t *testing.T) {
	fx := buildCrossModuleGenericFixture(t)

	// The i32 key is requested from three consumer sites (two call sites and a
	// bare generic value) and the char key from two; the final buildSpecializations
	// pass also re-requests every solved instantiation. None of that may add
	// more than one FunctionDeclaration per key.
	declarations := fx.specializationDeclarations()
	if len(declarations) != 2 {
		t.Fatalf("specialization declarations = %d, want exactly 2 despite repeated same-key requests", len(declarations))
	}
	for _, node := range declarations {
		if len(node.TypeArgs) != 1 || len(node.Parameters) != 1 || node.Parameters[0].Type != node.TypeArgs[0] {
			t.Fatalf("specialization declaration = %+v, want one fully substituted parameter", node)
		}
	}

	// Each bare generic value site contributes exactly one instantiation-table
	// entry, all owned by the declaring generic symbol, and no repeated
	// same-key build request appends a duplicate entry.
	var bareValues []tir.Node
	for _, node := range fx.unit.Nodes() {
		if node.Kind == tir.GenericFunctionValue && node.Symbol == fx.generic {
			bareValues = append(bareValues, node)
		}
	}
	if len(bareValues) != 2 {
		t.Fatalf("GenericFunctionValue nodes = %d, want 2", len(bareValues))
	}
	table := fx.unit.Instantiations()
	if len(table) != len(bareValues) {
		t.Fatalf("instantiation-table entries = %d, want %d (one per bare value site)", len(table), len(bareValues))
	}
	seenSites := make(map[symbol.SyntaxRef]bool)
	for _, entry := range table {
		if entry.Declaration != fx.generic {
			t.Fatalf("instantiation-table entry Declaration = %d, want imported generic %d", entry.Declaration, fx.generic)
		}
		if seenSites[entry.Site] {
			t.Fatalf("duplicate instantiation-table entry for site %+v", entry.Site)
		}
		seenSites[entry.Site] = true
	}
	for _, node := range bareValues {
		if uint64(node.GenericRef) >= uint64(len(table)) {
			t.Fatalf("GenericFunctionValue GenericRef %d out of instantiation-table range %d", node.GenericRef, len(table))
		}
		if entry := table[node.GenericRef]; entry.Declaration != fx.generic || !equalTypeArgs(entry.TypeArgs, node.TypeArgs) {
			t.Fatalf("GenericFunctionValue reference %d points to %+v, want the generic's %v", node.GenericRef, entry, node.TypeArgs)
		}
	}
}
