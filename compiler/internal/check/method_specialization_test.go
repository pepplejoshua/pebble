package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

// methodSymbolByContaining finds the method member symbol of the nominal type
// named ownerName whose own name is methodName.
func methodSymbolByContaining(t *testing.T, resolution *symbol.Result, ownerName, methodName string) symbol.SymbolID {
	t.Helper()
	var owner symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == ownerName && candidate.Kind == symbol.SymbolType {
			owner = candidate.ID
			break
		}
	}
	if owner == 0 {
		t.Fatalf("missing type %q", ownerName)
	}
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == methodName && candidate.Kind == symbol.SymbolMethod && candidate.Containing == owner {
			return candidate.ID
		}
	}
	t.Fatalf("missing method %q on %q", methodName, ownerName)
	return 0
}

// methodCallNodes returns every tir.MethodCall node in the unit.
func methodCallNodes(unit *tir.Unit) []tir.Node {
	var calls []tir.Node
	for _, node := range unit.Nodes() {
		if node.Kind == tir.MethodCall {
			calls = append(calls, node)
		}
	}
	return calls
}

// methodSpecializations returns every FunctionDeclaration node in the unit that
// specializes methodSymbol (same symbol, non-empty TypeArgs), in node order.
func methodSpecializations(unit *tir.Unit, methodSymbol symbol.SymbolID) []tir.Node {
	var declarations []tir.Node
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FunctionDeclaration && node.Symbol == methodSymbol && len(node.TypeArgs) != 0 {
			declarations = append(declarations, node)
		}
	}
	return declarations
}

func TestBuildUnitBuildsGenericMethodSpecialization(t *testing.T) {
	// The exact motivating repro: a generic struct method redeclaring the
	// struct's own type parameter on itself (`fn get[K](self Box[K]) K`),
	// called via ordinary method-call syntax with the type argument inferred
	// from the receiver. The checker must build a concrete specialization for
	// it exactly like it does for a free generic function call, or the
	// backend's findCalledFunctionDeclaration finds no decl to lower.
	unit, ok := buildUnitFixture(t, `
type Box[K] = struct {
    value K;
    fn get[K](self Box[K]) K {
        return self.value;
    }
};
fn main() int {
    var b Box[int] = Box[int].{ value = 5 };
    return b.get();
}
`)
	if !ok || unit == nil {
		t.Fatal("generic method call fixture was rejected")
	}
	inputs, _ := factInputs(t, checkProvider{"main.peb": []byte(`
type Box[K] = struct {
    value K;
    fn get[K](self Box[K]) K {
        return self.value;
    }
};
fn main() int {
    var b Box[int] = Box[int].{ value = 5 };
    return b.get();
}
`)})
	getSymbol := methodSymbolByContaining(t, inputs.Resolution, "Box", "get")
	calls := methodCallNodes(unit)
	if len(calls) != 1 {
		t.Fatalf("MethodCall nodes = %d, want exactly one", len(calls))
	}
	call := calls[0]
	if len(call.TypeArgs) != 2 {
		t.Fatalf("MethodCall TypeArgs = %v, want the receiver-bound plus own argument [int, int]", call.TypeArgs)
	}
	declarations := methodSpecializations(unit, getSymbol)
	if len(declarations) != 1 {
		t.Fatalf("method specializations = %d, want exactly one", len(declarations))
	}
	declaration := declarations[0]
	if declaration.Function == 0 || len(declaration.Parameters) != 1 || len(declaration.TypeArgs) != 2 {
		t.Fatalf("specialized method declaration = %+v", declaration)
	}
	if len(call.TypeArgs) != len(declaration.TypeArgs) {
		t.Fatalf("MethodCall TypeArgs = %v do not match specialization TypeArgs %v", call.TypeArgs, declaration.TypeArgs)
	}
	for i := range call.TypeArgs {
		if call.TypeArgs[i] != declaration.TypeArgs[i] {
			t.Fatalf("MethodCall TypeArgs = %v do not match specialization TypeArgs %v", call.TypeArgs, declaration.TypeArgs)
		}
	}
}

func TestBuildUnitGenericMethodDistinctSpecializations(t *testing.T) {
	// TWO specializations of the same generic struct calling the SAME method
	// in one program must each get their OWN method specialization: Box[int]'s
	// get returns int, Box[bool]'s returns bool, and the two FunctionDeclaration
	// nodes must be distinct (shared-symbol-across-specializations discipline).
	unit, ok := buildUnitFixture(t, `
type Box[K] = struct {
    value K;
    fn get[K](self Box[K]) K {
        return self.value;
    }
};
fn main() int {
    var b Box[int] = Box[int].{ value = 5 };
    var c Box[bool] = Box[bool].{ value = true };
    if c.get() {
        return b.get();
    }
    return 0;
}
`)
	if !ok || unit == nil {
		t.Fatal("two-specialization method fixture was rejected")
	}
	inputs, _ := factInputs(t, checkProvider{"main.peb": []byte(`
type Box[K] = struct {
    value K;
    fn get[K](self Box[K]) K {
        return self.value;
    }
};
fn main() int {
    var b Box[int] = Box[int].{ value = 5 };
    var c Box[bool] = Box[bool].{ value = true };
    if c.get() {
        return b.get();
    }
    return 0;
}
`)})
	getSymbol := methodSymbolByContaining(t, inputs.Resolution, "Box", "get")
	calls := methodCallNodes(unit)
	if len(calls) != 2 {
		t.Fatalf("MethodCall nodes = %d, want two", len(calls))
	}
	declarations := methodSpecializations(unit, getSymbol)
	if len(declarations) != 2 {
		t.Fatalf("method specializations = %d, want two distinct instantiations", len(declarations))
	}
	if declarations[0].Function == declarations[1].Function {
		t.Fatal("two method specializations share one FunctionID")
	}
	if len(declarations[0].TypeArgs) != 2 || len(declarations[1].TypeArgs) != 2 {
		t.Fatalf("specialization TypeArgs = %v and %v, want two arguments each", declarations[0].TypeArgs, declarations[1].TypeArgs)
	}
	if declarations[0].TypeArgs[0] == declarations[1].TypeArgs[0] {
		t.Fatalf("specialization TypeArgs = %v and %v, want distinct receiver-bound arguments", declarations[0].TypeArgs, declarations[1].TypeArgs)
	}
	for _, call := range calls {
		matched := false
		for _, declaration := range declarations {
			if len(call.TypeArgs) == len(declaration.TypeArgs) {
				equal := true
				for i := range call.TypeArgs {
					if call.TypeArgs[i] != declaration.TypeArgs[i] {
						equal = false
						break
					}
				}
				matched = matched || equal
			}
		}
		if !matched {
			t.Fatalf("MethodCall TypeArgs %v match no specialization", call.TypeArgs)
		}
	}
}

func TestBuildUnitGenericMethodParameterTypes(t *testing.T) {
	// A method taking parameters beyond self that also depend on the type
	// parameters (mirroring std/hmap.peb's insert(self, key K, value V)) must
	// resolve those parameter types, not just the self receiver and result,
	// in the built specialization.
	unit, ok := buildUnitFixture(t, `
type Pair[K, V] = struct {
    key K;
    value V;
    fn put[K, V](self *Pair[K, V], k K, v V) K {
        self.key = k;
        self.value = v;
        return self.key;
    }
};
fn main() int {
    var p Pair[int, int] = Pair[int, int].{ key = 1, value = 2 };
    let got int = p.put(4, 5);
    return got;
}
`)
	if !ok || unit == nil {
		t.Fatal("multi-parameter generic method fixture was rejected")
	}
	inputs, _ := factInputs(t, checkProvider{"main.peb": []byte(`
type Pair[K, V] = struct {
    key K;
    value V;
    fn put[K, V](self *Pair[K, V], k K, v V) K {
        self.key = k;
        self.value = v;
        return self.key;
    }
};
fn main() int {
    var p Pair[int, int] = Pair[int, int].{ key = 1, value = 2 };
    let got int = p.put(4, 5);
    return got;
}
`)})
	putSymbol := methodSymbolByContaining(t, inputs.Resolution, "Pair", "put")
	declarations := methodSpecializations(unit, putSymbol)
	if len(declarations) != 1 {
		t.Fatalf("put specializations = %d, want exactly one", len(declarations))
	}
	declaration := declarations[0]
	if len(declaration.Parameters) != 3 {
		t.Fatalf("specialized put parameters = %+v, want self, k, v", declaration.Parameters)
	}
	intType := inputs.Types.Builtins().Int
	// The two extra parameters must both be substituted to the concrete int,
	// and the pointer-typed self receiver must be distinct from them.
	if declaration.Parameters[1].Type != intType || declaration.Parameters[2].Type != intType {
		t.Fatalf("specialized put value parameter types = %d, %d, want int %d", declaration.Parameters[1].Type, declaration.Parameters[2].Type, intType)
	}
	if declaration.Parameters[0].Type == intType {
		t.Fatalf("specialized put self parameter type %d must not collapse to the value parameter type", declaration.Parameters[0].Type)
	}
	if len(declaration.TypeArgs) != 4 {
		t.Fatalf("specialized put TypeArgs = %v, want receiver-bound [int, int] plus own [int, int]", declaration.TypeArgs)
	}
	for i, argument := range declaration.TypeArgs {
		if argument != intType {
			t.Fatalf("specialized put TypeArgs[%d] = %d, want int %d", i, argument, intType)
		}
	}
}
