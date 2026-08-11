package check

import (
	"os"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func TestCallFactsDirectIndirectMethodAndSlots(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn add(left i32, right i32) i32 => left;
extern fn foreign(value i32) i32;
type Box = struct { value i32; fn get(self Box) i32 => self.value; fn echo[T](self Box, value T) T => value; };
type Choice = union enum { empty void; value i32; };
let direct i32 = add(1, 2);
let function fn(i32, i32) i32 = add;
let indirect i32 = function(3, 4);
let box Box = Box.{ value = 1 };
let method i32 = box.get();
let generic_method i32 = box.echo(7);
let explicit_method i32 = box.echo[i32](8);
let variant Choice = Choice.value(5);
let c_call i32 = foreign(6);
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	counts := map[callKind]int{}
	joinedCalls := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Kind == expressionCall && retained.Expression.Specialized != 0 {
			joinedCalls++
		}
		if retained.Call == nil {
			continue
		}
		counts[retained.Call.Target.Kind]++
		for _, argument := range retained.Call.Arguments {
			root, ok := facts.Generation.roots.root(argument.Destination)
			if !ok || root.Kind != rootSlot || root.Alternative != retained.Call.Header.Alternative {
				t.Fatalf("argument root = %+v, %v", root, ok)
			}
		}
	}
	if counts[callDirect] != 2 || counts[callIndirect] != 2 || counts[callMethod] != 3 || counts[callVariant] != 1 {
		t.Fatalf("call counts = %+v diagnostics=%+v", counts, diagnostics.Items())
	}
	if joinedCalls != 8 {
		t.Fatalf("joined calls=%d", joinedCalls)
	}
	flows := map[contextFlowKind]int{}
	for _, retained := range facts.Generation.records.values {
		if retained.ContextFlow != nil && retained.ContextFlow.Header.Syntax.Node != 0 {
			flows[retained.ContextFlow.Kind]++
		}
	}
	if flows[contextForward] == 0 || flows[contextNone] == 0 || flows[contextIndirect] == 0 {
		t.Fatalf("context flows=%+v", flows)
	}
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
	for _, retained := range facts.Generation.records.values {
		if retained.Call != nil && retained.Call.Target.Kind == callMethod {
			if selected, ok := solution.Method(retained.Call.Target.Site); !ok || selected.Method == 0 {
				t.Fatal("method identity not supplied by Solution.Method")
			}
		}
	}
}

func TestCallFactsAutoReferencesValueForPointerReceiver(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type S = struct {
    n i32;
    fn set(self *S, value i32) void { self.n = value; }
    fn touch(self *S) void { }
};
fn make() S => S.{ n = 0 };
fn copied(s S) void { var mutable = s; mutable.set(1); mutable.touch(); }
fn use() void {
    var s = S.{ n = 0 };
    s.set(1);
    s.touch();
    var made = make();
    made.set(2);
    let p *S = &s;
    p.set(3);
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestCallFactsGenericPointerReceiverCallsSiblingMethod(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Vec[T] = struct {
    value T;
    fn reserve[T](self *Vec[T], amount T) void { self.value = amount; }
    fn push[T](self *Vec[T], value T) void { self.reserve(value); }
};
fn use() void { var v = Vec[i32].{ value = 0 }; v.push(1); }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestCallFactsValueReceiverStillWorks(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type S = struct { n i32; fn read(self S) i32 => self.n; };
fn use() i32 { let s = S.{ n = 7 }; return s.read(); }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	if solution := facts.Session.Solve(); !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
}

func TestCallFactsFunctionFieldsUseMethodSyntax(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn add(value i32) i32 => value;
type Box = struct {
    callback fn(i32) i32;
    fn get(self Box, value i32) i32 => self.callback(value);
};
let callback_box Box = Box.{ callback = add };
let field_result i32 = callback_box.callback(7);
let method_result i32 = callback_box.get(8);
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("function field or real method did not solve: %+v", diagnostics.Items())
	}
	methodCalls, selectedMethods := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Call != nil && retained.Call.Target.Kind == callMethod {
			methodCalls++
			if _, ok := solution.Method(retained.Call.Target.Site); ok {
				selectedMethods++
			}
		}
	}
	if methodCalls != 3 || selectedMethods != 1 {
		t.Fatalf("method-shaped calls = %d selected methods = %d", methodCalls, selectedMethods)
	}
}

func TestCallFactsFunctionFieldCallRejectsUnknownMember(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { callback fn(i32) i32; };
fn main() void { let box Box = Box.{}; let value i32 = box.missing(1); }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	facts.Session.Solve()
	if !diagnostics.HasErrors() {
		t.Fatal("unknown member call was accepted")
	}
}

func TestCallFactsRecoveryKeepsIndependentCall(t *testing.T) {
	contents, err := os.ReadFile("../../../tests/check/facts/recovery/call_independent.peb")
	if err != nil {
		t.Fatal(err)
	}
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": contents})
	facts := run06a3(inputs, diagnostics, Config{})
	direct := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Call != nil && retained.Call.Target.Kind == callDirect {
			direct++
		}
	}
	if direct != 1 || !diagnostics.HasErrors() {
		t.Fatalf("direct=%d diagnostics=%+v", direct, diagnostics.Items())
	}
}

func TestDeferredBracketDownstreamCallsStayInTheirAlternatives(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
type Box = struct {
    functions [1]fn(i32) i32;
    fn echo[T](self Box, value T) T => value;
};
let function fn(i32) i32 = identity[i32];
let box Box = Box.{ functions = [function] };
let method i32 = box.echo[i32](1);
let indexed i32 = box.functions[0](2);
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("diagnostics=%+v", diagnostics.Items())
	}
	activeMethod, activeIndirect := 0, 0
	inactiveMethod, inactiveIndirect := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Call == nil || !retained.Header.Alternative.Guarded {
			continue
		}
		selection, ok := solution.Selection(retained.Header.Alternative.Choice)
		if !ok {
			t.Fatalf("guarded call has no selection: %+v", retained.Header)
		}
		active := selection == retained.Header.Alternative.Index
		switch retained.Call.Target.Kind {
		case callMethod:
			if active {
				activeMethod++
				if selected, found := solution.Method(retained.Call.Target.Site); !found || selected.Method == 0 || retained.Call.Target.Symbol != 0 {
					t.Fatalf("method identity=%+v found=%v record=%+v", selected, found, retained.Call)
				}
			} else {
				inactiveMethod++
			}
		case callIndirect:
			if active {
				activeIndirect++
			} else {
				inactiveIndirect++
			}
		}
	}
	if activeMethod != 1 || activeIndirect != 1 || inactiveMethod != 1 || inactiveIndirect != 1 {
		t.Fatalf("active method/indirect=%d/%d inactive=%d/%d", activeMethod, activeIndirect, inactiveMethod, inactiveIndirect)
	}
	for _, root := range facts.Generation.roots.values {
		if !root.Root.Alternative.Guarded {
			continue
		}
		selection, selected := solution.Selection(root.Root.Alternative.Choice)
		_, visible := solution.Slot(root.Root.Slot)
		if !selected || visible != (selection == root.Root.Alternative.Index) {
			t.Fatalf("guarded call root leaked: selection=%d/%v root=%+v visible=%v", selection, selected, root.Root, visible)
		}
	}
}

func TestMemberFactsUseAuthoritativeIdentity(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Box = struct { value i32; fn get(self Box) i32 => self.value; };
type Color = enum { red, blue };
let box Box = Box.{ value = 1 };
let field i32 = box.value;
let method i32 = box.get();
let color Color = Color.red;
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	field, method, variant, joined := false, false, false, false
	for _, retained := range facts.Generation.records.values {
		if retained.Expression != nil && retained.Expression.Kind == expressionMember && retained.Expression.Specialized != 0 {
			joined = true
		}
		if retained.Member == nil {
			continue
		}
		field = field || retained.Member.Kind == memberField
		if retained.Member.Kind == memberMethod {
			method = retained.Member.Member == 0
		}
		variant = variant || retained.Member.Kind == memberVariant && retained.Member.Member != 0
	}
	if !field || !method || !variant || !joined {
		t.Fatalf("field/method/variant=%v/%v/%v diagnostics=%+v", field, method, variant, diagnostics.Items())
	}
}

func TestCallMemberIndexRequirementRecordRejectionIsAtomic(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn identity[T](value T) T => value;
fn add(value i32) i32 => value;
type Box = struct { value i32; fn get(self Box) i32 => self.value; };
type Choice = union enum { value i32; };
let function fn(i32) i32 = add;
let box Box = Box.{ value = 1 };
let direct i32 = identity[i32](1);
let indirect i32 = function(1);
let method i32 = box.get();
let variant Choice = Choice.value(1);
let field i32 = box.value;
let tuple (i32, i32) = (1, 2);
let first i32 = tuple.0;
let indexed char = box.value[0];
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	reject := func(name string, source retainedRecord, mutate func(*retainedRecord)) {
		t.Helper()
		candidate := cloneRetainedRecord(source)
		header := candidate.Header
		header.ID = 0
		candidate.assignHeader(header)
		mutate(&candidate)
		beforeRecords := len(facts.Generation.records.values)
		beforeComponents := facts.Generation.records.components
		if id, ok := facts.Generation.addRecord(candidate); ok || id != 0 {
			t.Fatalf("%s invalid record accepted: id=%d", name, id)
		}
		if len(facts.Generation.records.values) != beforeRecords || facts.Generation.records.components != beforeComponents {
			t.Fatalf("%s rejection was not atomic", name)
		}
	}
	callKinds := map[callKind]bool{}
	memberKinds := map[memberKind]bool{}
	indexFound := false
	for _, retained := range append([]retainedRecord(nil), facts.Generation.records.values...) {
		if retained.Call != nil && !callKinds[retained.Call.Target.Kind] {
			kind := retained.Call.Target.Kind
			callKinds[kind] = true
			reject("call", retained, func(candidate *retainedRecord) {
				switch kind {
				case callDirect, callVariant:
					candidate.Call.Target.Symbol = 0
				case callMethod:
					candidate.Call.Target.Site = symbol.SyntaxRef{}
				case callIndirect:
					candidate.Call.Callee = 0
				}
			})
		}
		if retained.Member != nil && !memberKinds[retained.Member.Kind] {
			kind := retained.Member.Kind
			memberKinds[kind] = true
			reject("member", retained, func(candidate *retainedRecord) {
				if kind == memberStatic || kind == memberVariant {
					candidate.Member.Member = 0
				} else {
					candidate.Member.Base = 0
				}
			})
		}
		if retained.Index != nil && !indexFound {
			indexFound = true
			reject("index", retained, func(candidate *retainedRecord) { candidate.Index.Base = 0 })
		}
	}
	if len(callKinds) != 4 || len(memberKinds) < 3 || !indexFound {
		t.Fatalf("record coverage calls=%+v members=%+v index=%v diagnostics=%+v", callKinds, memberKinds, indexFound, diagnostics.Items())
	}

	requirementInputs, requirementDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn inspect[T](value T) void { let field = value.unknown; }
`)})
	requirementFacts := run06a3(requirementInputs, requirementDiagnostics, Config{})
	for _, retained := range append([]retainedRecord(nil), requirementFacts.Generation.records.values...) {
		if retained.Requirement == nil {
			continue
		}
		candidate := cloneRetainedRecord(retained)
		header := candidate.Header
		header.ID = 0
		candidate.assignHeader(header)
		candidate.Requirement.Subject = 0
		beforeRecords := len(requirementFacts.Generation.records.values)
		beforeComponents := requirementFacts.Generation.records.components
		if id, ok := requirementFacts.Generation.addRecord(candidate); ok || id != 0 || len(requirementFacts.Generation.records.values) != beforeRecords || requirementFacts.Generation.records.components != beforeComponents {
			t.Fatalf("requirement rejection was not atomic: id=%d ok=%v", id, ok)
		}
		return
	}
	t.Fatalf("requirement payload not generated: %+v", requirementDiagnostics.Items())
}

// A static or variant member is exactly its resolved symbol term. The member
// rule must not allocate a session variable it then discards on those paths:
// the abandoned variable is never constrained and reports a spurious T0510.
func TestMemberFactsStaticAndVariantLeakNoInferenceVariable(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, green, blue };
type Choice = union enum { empty void; value i32; };
fn f() void {
    let shade Color = Color.red;
    let taken Choice = Choice.value(2);
}
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("payloadless variant in value position did not solve: %+v", diagnostics.Items())
	}
	variants := 0
	for _, retained := range facts.Generation.records.values {
		if retained.Member == nil || retained.Member.Kind != memberVariant {
			continue
		}
		variants++
		if retained.Member.Member == 0 {
			t.Fatalf("variant member record has no resolved identity: %+v", retained.Member)
		}
	}
	// Only the payloadless variant reaches the member rule; a tagged variant
	// construction is a callVariant site and owns a call record instead.
	if variants != 1 {
		t.Fatalf("variant member records = %d", variants)
	}
}

// TestCallFactsQualifiedSelflessMethodClassifiesAsStaticDirect pins the static
// method feature: `Point.origin()` where origin is a method declared inside
// the struct body with no self parameter must resolve as a plain direct call to
// origin's own function symbol, with no implicit receiver and the authored
// arguments exactly as written. Before the fix, staticTarget had no SymbolMethod
// case, so the call was misclassified as callMethod with no receiver and the
// call record was rejected by the C0619 semantic-record gate.
func TestCallFactsQualifiedSelflessMethodClassifiesAsStaticDirect(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct {
    x i32;
    fn origin(offset i32) Point { return Point.{ x = offset }; }
};
fn use() i32 { let p = Point.origin(1); return p.x; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if !solution.Successful() || diagnostics.HasErrors() {
		t.Fatalf("qualified self-less method call did not solve: %+v", diagnostics.Items())
	}
	originID := symbol.SymbolID(0)
	for _, sym := range inputs.Resolution.Symbols.All() {
		if sym.Kind == symbol.SymbolMethod && sym.Name == "origin" {
			originID = sym.ID
		}
	}
	if originID == 0 {
		t.Fatal("origin method symbol was not collected")
	}
	staticCalls, methodCalls := 0, 0
	for _, retained := range facts.Generation.records.values {
		if retained.Call == nil {
			continue
		}
		if retained.Call.Target.Kind == callMethod {
			methodCalls++
		}
		if retained.Call.Target.Kind != callDirect || retained.Call.Target.Symbol != originID {
			continue
		}
		staticCalls++
		if retained.Call.Receiver != 0 {
			t.Fatalf("static call must have no receiver: %+v", retained.Call)
		}
		if len(retained.Call.Arguments) != 1 || retained.Call.Arguments[0].Ordinal != 0 {
			t.Fatalf("static call must carry the authored argument exactly as written: %+v", retained.Call)
		}
	}
	if staticCalls != 1 || methodCalls != 0 {
		t.Fatalf("static direct calls = %d, method calls = %d, want 1 and 0", staticCalls, methodCalls)
	}
}

// TestCallFactsSelflessMethodInvokedOnInstanceRejects pins the rejection side:
// a self-less static method called with ordinary instance-method syntax on a
// value (`p.origin()`) must still be rejected — the method selection path
// demands a self-typed first parameter, so the mismatch fails the solve
// (T0505) rather than silently invoking the static function on the receiver.
func TestCallFactsSelflessMethodInvokedOnInstanceRejects(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Point = struct {
    x i32;
    fn origin() Point { return Point.{ x = 0 }; }
};
fn use() i32 { let p = Point.{ x = 1 }; let q = p.origin(); return q.x; }
`)})
	facts := run06a3(inputs, diagnostics, Config{})
	solution := facts.Session.Solve()
	if solution.Successful() && !diagnostics.HasErrors() {
		t.Fatal("self-less static method invoked as an instance method was accepted")
	}
}

// TestCallFactsSoleSliceTailArgumentDestinationIsSliceType pins the V1-parity
// forward case: a variadic call whose tail has exactly ONE argument whose own
// statically-known declared type equals the variadic parameter's whole slice
// type binds that argument against the SLICE type (the whole slice is
// forwarded, matching V1's `arg_count == required_param_count + 1` branch),
// not against the per-element type. It must resolve to []int, never int.
func TestCallFactsSoleSliceTailArgumentDestinationIsSliceType(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn sum(...values []int) int {
    var total int = 0;
    var i uint = 0;
    while i < values.len {
        total = total + values[i];
        i = i + 1;
    }
    return total;
}
fn main() int {
    var arr [3]int = [1, 2, 3];
    var s []int = arr[0:3];
    return sum(s);
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid fixture: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	sliceType, err := inputs.Types.Intern(types.SliceKey(inputs.Types.Builtins().Int))
	if err != nil {
		t.Fatal(err)
	}
	for _, retained := range handoff.Records.Records() {
		if retained.Call == nil {
			continue
		}
		for _, argument := range retained.Call.Arguments {
			if !argument.Variadic {
				continue
			}
			typ, ok := typeOfValue(records, argument.Destination)
			if !ok || typ != sliceType {
				t.Fatalf("sole variadic tail argument destination type = %d, want the whole slice type %d", typ, sliceType)
			}
		}
	}
}

// TestCallFactsSoleElementTailArgumentDestinationIsElementType pins the
// untouched single-element side of the same branch: a sole tail argument that
// is NOT a slice (`sum(5)`) keeps its per-element destination type (int), so a
// bare int literal remains a one-element variadic call exactly as before.
func TestCallFactsSoleElementTailArgumentDestinationIsElementType(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn sum(...values []int) int {
    var total int = 0;
    var i uint = 0;
    while i < values.len {
        total = total + values[i];
        i = i + 1;
    }
    return total;
}
fn main() int { return sum(5); }
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid fixture: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	intType := inputs.Types.Builtins().Int
	for _, retained := range handoff.Records.Records() {
		if retained.Call == nil {
			continue
		}
		for _, argument := range retained.Call.Arguments {
			if !argument.Variadic {
				continue
			}
			typ, ok := typeOfValue(records, argument.Destination)
			if !ok || typ != intType {
				t.Fatalf("sole element tail argument destination type = %d, want the element type %d", typ, intType)
			}
		}
	}
}

// TestCallFactsMultipleTailArgumentsDestinationsAreElementType pins that the
// forward case applies ONLY to a sole tail argument: two or more tail
// arguments keep per-element destinations, so `sum(1, 2, 3)` binds each
// argument against int.
func TestCallFactsMultipleTailArgumentsDestinationsAreElementType(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
fn sum(...values []int) int {
    var total int = 0;
    var i uint = 0;
    while i < values.len {
        total = total + values[i];
        i = i + 1;
    }
    return total;
}
fn main() int { return sum(1, 2, 3); }
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.GenerationHadErrors {
		t.Fatalf("invalid fixture: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatal(diagnostics.Items())
	}
	intType := inputs.Types.Builtins().Int
	for _, retained := range handoff.Records.Records() {
		if retained.Call == nil {
			continue
		}
		for _, argument := range retained.Call.Arguments {
			if !argument.Variadic {
				continue
			}
			typ, ok := typeOfValue(records, argument.Destination)
			if !ok || typ != intType {
				t.Fatalf("tail argument %d destination type = %d, want the element type %d", argument.Ordinal, typ, intType)
			}
		}
	}
}
