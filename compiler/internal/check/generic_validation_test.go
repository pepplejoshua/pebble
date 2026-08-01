package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
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
