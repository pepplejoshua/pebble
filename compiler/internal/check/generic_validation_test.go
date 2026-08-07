package check

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
)

func checkGenericFixture(t *testing.T, source string) (*Result, *diagnostic.DiagnosticSet) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	return Check(inputs, diagnostics, Config{}), diagnostics
}

func TestGenericInstantiationOrderingRejectsUnorderedType(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Pair = struct { value i32; };
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn check() void {
	let left Pair = Pair.{ value = 1 };
	let right Pair = Pair.{ value = 2 };
	let result Pair = max(left, right);
}
`)})
	result := Check(inputs, diagnostics, Config{})
	if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("unordered instantiation was accepted: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

func TestGenericInstantiationOrderingAcceptsOrderedType(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn check() void { let result i32 = max(1, 2); }
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("ordered instantiation was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

func TestGenericSizeofTypeParameter(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{"concrete instantiation", `fn f[T]() uint { return sizeof T; } fn main() i32 { return f[i32]() as i32; }`, true},
		{"uninstantiated template", `fn f[T]() uint { return sizeof T; }`, true},
		{"invalid concrete function type", `fn g[T]() uint { return sizeof T; } fn f() void {} fn main() i32 { return g[fn() void]() as i32; }`, false},
		{"generic nominal type", `type Entry[K, V] = struct { key K; value V; }; fn size[K, V]() uint { return sizeof Entry[K, V]; } fn main() i32 { return size[i32, i64]() as i32; }`, true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, diagnostics := checkGenericFixture(t, test.source)
			if result.Successful() != test.valid || (!test.valid && !hasValidationDiagnostic(diagnostics, CodeAggregate)) {
				t.Fatalf("sizeof generic validity mismatch: valid=%v result=%v diagnostics=%+v", test.valid, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestGenericInstantiationEquatable(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{"valid", `fn same[T](a T, b T) bool { return a == b; } fn check() void { let a i32 = 1; let b i32 = 2; let result bool = same[i32](a, b); }`, true},
		{"invalid", `type Pair = struct { value i32; }; fn same[T](a T, b T) bool { return a == b; } fn check() void { let left Pair = Pair.{ value = 1 }; let right Pair = Pair.{ value = 2 }; let result bool = same(left, right); }`, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, diagnostics := checkGenericFixture(t, test.source)
			if result.Successful() != test.valid || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) == test.valid {
				t.Fatalf("equatable instantiation mismatch: valid=%v result=%v diagnostics=%+v", test.valid, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestGenericInstantiationNumeric(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{"valid", `fn add[T](a T, b T) T { return a + b; } fn check() void { let result i32 = add(1, 2); }`, true},
		{"invalid", `type Pair = struct { value i32; }; fn add[T](a T, b T) T { return a + b; } fn check() void { let left Pair = Pair.{ value = 1 }; let right Pair = Pair.{ value = 2 }; let result Pair = add(left, right); }`, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, diagnostics := checkGenericFixture(t, test.source)
			if result.Successful() != test.valid || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) == test.valid {
				t.Fatalf("numeric instantiation mismatch: valid=%v result=%v diagnostics=%+v", test.valid, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestGenericInstantiationIntegral(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{"valid", `fn shift[T](a T, b T) T { return a << b; } fn check() void { let result i32 = shift(4, 1); }`, true},
		{"invalid", `fn shift[T](a T, b T) T { return a << b; } fn check() void { let result f64 = shift(4.0, 1.0); }`, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, diagnostics := checkGenericFixture(t, test.source)
			if result.Successful() != test.valid || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) == test.valid {
				t.Fatalf("integral instantiation mismatch: valid=%v result=%v diagnostics=%+v", test.valid, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestGenericInstantiationLiteralFits(t *testing.T) {
	tests := []struct {
		name   string
		source string
		valid  bool
	}{
		{"integer fits", `fn overflow[T](value T) T { return value + 1; } fn check() void { let result i8 = overflow[i8](127); }`, true},
		{"integer does not fit i8", `fn overflow[T](value T) T { return value + 200; } fn check() void { let result i8 = overflow[i8](1); }`, false},
		{"integer fits i32", `fn overflow[T](value T) T { return value + 200; } fn check() void { let result i32 = overflow[i32](1); }`, true},
		{"float fits f32", `fn overflow[T](value T) T { return value + 1.5; } fn check() void { let result f32 = overflow[f32](0.5); }`, true},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			result, diagnostics := checkGenericFixture(t, test.source)
			if result.Successful() != test.valid || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) == test.valid {
				t.Fatalf("literal-fits instantiation mismatch: valid=%v result=%v diagnostics=%+v", test.valid, result.Successful(), diagnostics.Items())
			}
		})
	}
}

func TestGenericInstantiationChecksEachCallSite(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
type Pair = struct { value i32; };
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn check() void {
	let good i32 = max(1, 2);
	let left Pair = Pair.{ value = 1 };
	let right Pair = Pair.{ value = 2 };
	let bad Pair = max(left, right);
}
`)
	count := 0
	for _, item := range diagnostics.Items() {
		if item.Code == CodeGenericInstantiation {
			count++
		}
	}
	if result.Successful() || count != 1 {
		t.Fatalf("expected one failing call site, result=%v count=%d diagnostics=%+v", result.Successful(), count, diagnostics.Items())
	}
}

// TestGenericTransitiveRequirementPropagation covers a generic function whose
// only use of a requirement-bearing operator is through calling ANOTHER generic
// function with its own type parameter, never with the operator directly. The
// callee's requirement must be propagated onto the caller's own type parameter
// so the caller's external instantiation sites are checked, and the caller's
// body-internal instantiations (whose arguments are the still-abstract type
// parameter) must be deferred rather than rejected. The typed IR must build so
// the program actually runs.
func TestGenericTransitiveRequirementPropagation(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
fn min[T](a T, b T) T { if a < b { return a; } return b; }
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn clamp[T](x T, lo T, hi T) T { return max(lo, min(x, hi)); }
fn check() void {
	let x i32 = 5;
	let lo i32 = 0;
	let hi i32 = 10;
	let r i32 = clamp(x, lo, hi);
}
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("clamp generic wrapper was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
	if result.IR() == nil {
		t.Fatalf("clamp generic wrapper produced no typed IR: %+v", diagnostics.Items())
	}
}

// TestGenericTransitiveRequirementChain proves the propagation is transitive
// through more than one generic hop: outer[T] calls mid[T] which calls min[T].
func TestGenericTransitiveRequirementChain(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
fn min[T](a T, b T) T { if a < b { return a; } return b; }
fn mid[T](a T, b T) T { return min(a, b); }
fn outer[T](a T, b T) T { return mid(a, b); }
fn check() void {
	let r i32 = outer(1, 2);
}
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("transitive generic chain was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
	if result.IR() == nil {
		t.Fatalf("transitive generic chain produced no typed IR: %+v", diagnostics.Items())
	}
}

// TestGenericTransitiveNumericRequirement proves the same propagation applies
// to a non-Ordered requirement kind (Numeric) reached only through a call.
func TestGenericTransitiveNumericRequirement(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
fn add[T](a T, b T) T { return a + b; }
fn double[T](x T) T { return add(x, x); }
fn check() void {
	let r i32 = double(5);
}
`)
	if !result.Successful() || hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("numeric generic wrapper was rejected: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

// TestGenericTransitiveRequirementRejectsUnordered is the regression guard that
// the fix does not disable the check entirely: a function shaped like clamp
// called with a concrete type that does not satisfy Ordered (a struct) must
// still fail with C0621.
func TestGenericTransitiveRequirementRejectsUnordered(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
type Pair = struct { value i32; };
fn min[T](a T, b T) T { if a < b { return a; } return b; }
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn clamp[T](x T, lo T, hi T) T { return max(lo, min(x, hi)); }
fn check() void {
	let x Pair = Pair.{ value = 5 };
	let lo Pair = Pair.{ value = 0 };
	let hi Pair = Pair.{ value = 10 };
	let r Pair = clamp(x, lo, hi);
}
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("unordered clamp instantiation was accepted: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

// TestGenericTransitiveRequirementRejectsNonNumeric guards the Numeric
// propagation the same way: a generic wrapper over an additive generic must
// still reject a non-numeric concrete type with C0621.
func TestGenericTransitiveRequirementRejectsNonNumeric(t *testing.T) {
	result, diagnostics := checkGenericFixture(t, `
type Pair = struct { value i32; };
fn add[T](a T, b T) T { return a + b; }
fn double[T](x T) T { return add(x, x); }
fn check() void {
	let x Pair = Pair.{ value = 5 };
	let r Pair = double(x);
}
`)
	if result.Successful() || !hasValidationDiagnostic(diagnostics, CodeGenericInstantiation) {
		t.Fatalf("non-numeric wrapper instantiation was accepted: result=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}
}

// expectedSourceSpan resolves a needle within the fixture source to the exact
// byte span the parser assigns to the authored construct. Spans are matched
// against the same source string handed to the provider, so byte offsets line
// up with the tree node spans carried by the retained records.
func expectedSourceSpan(t *testing.T, inputs Inputs, sourceText, needle string) source.Span {
	t.Helper()
	offset := strings.Index(sourceText, needle)
	if offset < 0 {
		t.Fatalf("needle %q not found in fixture source", needle)
	}
	root, ok := inputs.Graph.Module(inputs.Graph.Root)
	if !ok {
		t.Fatal("root module missing")
	}
	return source.NewSpan(root.Source, uint32(offset), uint32(offset+len(needle)))
}

// requireGenericInstantiation asserts the single C0621 diagnostic for a failing
// generic instantiation carries the expected message, primary span at the
// concrete failing site, and a related label at the generic-body origin.
func requireGenericInstantiation(t *testing.T, sourceText, primaryNeedle, relatedNeedle string) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(sourceText)})
	result := Check(inputs, diagnostics, Config{})
	if result.Successful() {
		t.Fatalf("failing generic instantiation was accepted: %+v", diagnostics.Items())
	}
	var found *diagnostic.Diagnostic
	for index := range diagnostics.Items() {
		item := diagnostics.Items()[index]
		if item.Code != CodeGenericInstantiation {
			continue
		}
		if found != nil {
			t.Fatalf("expected one generic instantiation diagnostic: %+v", diagnostics.Items())
		}
		found = &item
	}
	if found == nil {
		t.Fatalf("missing generic instantiation diagnostic: %+v", diagnostics.Items())
	}
	if want := "generic Ordered requirement failed at this instantiation site"; found.Message != want {
		t.Fatalf("message = %q, want %q", found.Message, want)
	}
	wantPrimary := expectedSourceSpan(t, inputs, sourceText, primaryNeedle)
	if found.Primary.Span != wantPrimary {
		t.Fatalf("primary span = %+v, want %+v covering %q", found.Primary.Span, wantPrimary, primaryNeedle)
	}
	if len(found.Related) != 1 {
		t.Fatalf("related labels = %d, want 1: %+v", len(found.Related), found.Related)
	}
	related := found.Related[0]
	if want := "generic Ordered requirement declared here"; related.Message != want {
		t.Fatalf("related message = %q, want %q", related.Message, want)
	}
	wantRelated := expectedSourceSpan(t, inputs, sourceText, relatedNeedle)
	if related.Span != wantRelated {
		t.Fatalf("related span = %+v, want %+v covering %q", related.Span, wantRelated, relatedNeedle)
	}
}

func TestGenericInstantiationCallSiteSpanAndRelated(t *testing.T) {
	requireGenericInstantiation(t, `
type Pair = struct { value i32; };
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn check() void {
	let left Pair = Pair.{ value = 1 };
	let right Pair = Pair.{ value = 2 };
	let result Pair = max(left, right);
}
`, "max(left, right)", "a > b")
}

func TestGenericInstantiationBareValueSpanAndRelated(t *testing.T) {
	requireGenericInstantiation(t, `
type Pair = struct { value i32; };
fn max[T](a T, b T) T { if a > b { return a; } return b; }
fn check() void {
	let f fn(Pair, Pair) Pair = max[Pair];
}
`, "max[Pair]", "a > b")
}

// requireGenericLiteralInstantiation asserts the single C0621 diagnostic for a
// failing literal-fits instantiation carries the LiteralFits message, a
// primary span at the concrete failing site, and a related label at the
// generic-body literal origin.
func requireGenericLiteralInstantiation(t *testing.T, sourceText, primaryNeedle, relatedNeedle string) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(sourceText)})
	result := Check(inputs, diagnostics, Config{})
	if result.Successful() {
		t.Fatalf("failing literal-fits instantiation was accepted: %+v", diagnostics.Items())
	}
	var found *diagnostic.Diagnostic
	for index := range diagnostics.Items() {
		item := diagnostics.Items()[index]
		if item.Code != CodeGenericInstantiation {
			continue
		}
		if found != nil {
			t.Fatalf("expected one generic instantiation diagnostic: %+v", diagnostics.Items())
		}
		found = &item
	}
	if found == nil {
		t.Fatalf("missing generic instantiation diagnostic: %+v", diagnostics.Items())
	}
	if want := "generic LiteralFits requirement failed at this instantiation site"; found.Message != want {
		t.Fatalf("message = %q, want %q", found.Message, want)
	}
	wantPrimary := expectedSourceSpan(t, inputs, sourceText, primaryNeedle)
	if found.Primary.Span != wantPrimary {
		t.Fatalf("primary span = %+v, want %+v covering %q", found.Primary.Span, wantPrimary, primaryNeedle)
	}
	if len(found.Related) != 1 {
		t.Fatalf("related labels = %d, want 1: %+v", len(found.Related), found.Related)
	}
	related := found.Related[0]
	if want := "generic LiteralFits requirement declared here"; related.Message != want {
		t.Fatalf("related message = %q, want %q", related.Message, want)
	}
	wantRelated := expectedSourceSpan(t, inputs, sourceText, relatedNeedle)
	if related.Span != wantRelated {
		t.Fatalf("related span = %+v, want %+v covering %q", related.Span, wantRelated, relatedNeedle)
	}
}

func TestGenericInstantiationLiteralFitsCallSiteSpanAndRelated(t *testing.T) {
	requireGenericLiteralInstantiation(t, `
fn overflow[T](value T) T { return value + 200; }
fn check() void {
	let result i8 = overflow[i8](1);
}
`, "overflow[i8](1)", "value + 200")
}

func TestGenericInstantiationLiteralFitsBareValueSpanAndRelated(t *testing.T) {
	requireGenericLiteralInstantiation(t, `
fn overflow[T](value T) T { return value + 200; }
fn check() void {
	let f fn(i8) i8 = overflow[i8];
}
`, "overflow[i8]", "value + 200")
}
