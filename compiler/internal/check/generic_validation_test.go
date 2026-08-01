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
