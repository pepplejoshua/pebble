package check

import (
	"os"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
)

// TestCheckPublishesTypedIRForValidProgram drives a real, non-trivial program
// (two functions, control flow, a call) through the public entry point and
// asserts the returned typed IR is the well-formed unit run06b built — the
// same construction ir_builder_test.go's buildUnitFixture tests, now reached
// through the real public path.
func TestCheckPublishesTypedIRForValidProgram(t *testing.T) {
	const source = `
fn helper(flag bool) i32 {
    if flag {
        return 1;
    } else {
        return 2;
    }
}

fn main() void {
    let total = helper(true);
    print total;
}
`
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	result := Check(inputs, diagnostics, Config{})
	if !result.Successful() {
		t.Fatalf("Check failed a valid program: %+v", diagnostics.Items())
	}
	if diagnostics.HasErrors() {
		t.Fatalf("valid program produced error diagnostics: %+v", diagnostics.Items())
	}

	unit := result.IR()
	if unit == nil {
		t.Fatal("IR() returned nil for a successful result")
	}
	if got := unit.FunctionCount(); got != 2 {
		t.Fatalf("FunctionCount = %d, want 2", got)
	}
	modules := unit.Modules()
	if len(modules) != 1 || modules[0].ID != 1 {
		t.Fatalf("Modules = %+v, want exactly the root module with ID 1", modules)
	}
	if unit.NodeCount() == 0 {
		t.Fatal("published IR has an empty node store")
	}
	if len(unit.SourceRefs()) == 0 {
		t.Fatal("published IR has an empty source map")
	}
	if ifNodes := nodesOfKind(unit, tir.If); len(ifNodes) != 1 {
		t.Fatalf("If nodes = %d, want 1", len(ifNodes))
	}
}

// TestCheckFailsEarlyValidationPublishesNoIR asserts that a program rejected
// by an earlier validation step (06a generation or a 06b.1-06b.6 validator)
// produces a failed Result with no typed IR.
func TestCheckFailsEarlyValidationPublishesNoIR(t *testing.T) {
	tests := []struct {
		name   string
		source string
		code   diagnostic.Code
	}{
		{
			name: "forbidden conversion C0601",
			code: CodeConversion,
			source: `
type Left = struct { value i32; };
type Right = struct { value i32; };
fn forbidden(value Left) void {
    let other Right = value;
}
`,
		},
		{
			name:   "missing initializer C0602",
			code:   CodeBindingInitializer,
			source: "let x i32;",
		},
		{
			name:   "nonconstant global C0616",
			code:   CodeNonconstantGlobal,
			source: "fn helper() i32 { return 1; }\nlet x i32 = helper();",
		},
		{
			name:   "discarded non-void value C0612",
			code:   CodeStatementForm,
			source: "fn main() void { 1; }",
		},
		{
			name:   "aggregate member C0605",
			code:   CodeMember,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0605/aggregate_unknown_field.peb"),
		},
		{
			name:   "call arity C0604",
			code:   CodeCall,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0604/call_arity_mismatch.peb"),
		},
		{
			name:   "callable capture C0617",
			code:   CodeCaptureViolation,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0617/callable_capturing_anonymous.peb"),
		},
		{
			name:   "index bound C0609",
			code:   CodeIndexBound,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0609/index_out_of_range.peb"),
		},
		{
			name:   "operator C0603",
			code:   CodeOperator,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0603/operator_unsigned_negate.peb"),
		},
		{
			name:   "place C0606",
			code:   CodePlace,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0606/nested_mutation.peb"),
		},
		{
			name:   "generic requirement C0610",
			code:   CodeUnsupportedGeneric,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0610/generic_unsupported_field.peb"),
		},
		{
			name:   "missing return C0607",
			code:   CodeMissingReturn,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0607/missing_return.peb"),
		},
		{
			name:   "switch target C0611",
			code:   CodeInvalidTarget,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0611/switch_duplicate.peb"),
		},
		{
			name:   "defer C0613",
			code:   CodeInvalidDefer,
			source: validationFixtureSource(t, "../../../tests/check/validation/invalid/C0613/defer_return.peb"),
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(tt.source)})
			result := Check(inputs, diagnostics, Config{})
			if result.Successful() {
				t.Fatalf("Check succeeded on invalid program: %+v", diagnostics.Items())
			}
			if result.IR() != nil {
				t.Fatal("a failed result must publish no IR")
			}
			if !hasValidationDiagnostic(diagnostics, tt.code) {
				t.Fatalf("expected diagnostic %s, got: %+v", tt.code, diagnostics.Items())
			}
		})
	}
}

func validationFixtureSource(t *testing.T, path string) string {
	t.Helper()
	contents, err := os.ReadFile(path)
	if err != nil {
		t.Fatalf("read validation fixture %s: %v", path, err)
	}
	return string(contents)
}

// TestCheckAllOrNothingIRGate proves the publication gate in both directions
// explicitly: a successful Result always publishes IR, and a failed Result
// never does.
func TestCheckAllOrNothingIRGate(t *testing.T) {
	tests := []struct {
		name   string
		source string
		wantOK bool
	}{
		{name: "empty main", source: "fn main() void {}", wantOK: true},
		{
			name: "functions and control flow",
			source: `
fn helper(flag bool) i32 {
    if flag {
        return 1;
    } else {
        return 2;
    }
}

fn main() void {
    let total = helper(true);
    print total;
}
`,
			wantOK: true,
		},
		{
			name: "generic body requirements",
			source: `
fn twice[T] (value T) T {
    return value + value;
}
`,
			wantOK: true,
		},
		{name: "forbidden conversion", source: `
type Left = struct { value i32; };
type Right = struct { value i32; };
fn forbidden(value Left) void {
    let other Right = value;
}
`},
		{name: "missing global initializer", source: "let x i32;"},
		{name: "nonconstant global", source: "fn helper() i32 { return 1; }\nlet x i32 = helper();"},
		{name: "discarded non-void value", source: "fn main() void { 1; }"},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(tt.source)})
			result := Check(inputs, diagnostics, Config{})
			if result.Successful() != tt.wantOK {
				t.Fatalf("Successful() = %v, want %v (diagnostics: %+v)", result.Successful(), tt.wantOK, diagnostics.Items())
			}
			if result.Successful() && result.IR() == nil {
				t.Fatal("invariant violated: Successful() == true but IR() == nil")
			}
			if !result.Successful() && result.IR() != nil {
				t.Fatal("invariant violated: Successful() == false but IR() != nil")
			}
		})
	}
}

// TestCheckIRConstructionGate confirms buildUnit is genuinely invoked by
// run06b and is itself a publication gate: a program that passes every
// 06b.1-06b.6 validator still fails the whole Result with no IR when typed-IR
// construction fails. MaxIRNodes is consumed only by buildUnit (via the tir
// builder), so lowering it to 1 isolates an IR-construction-only failure.
func TestCheckIRConstructionGate(t *testing.T) {
	const source = `
fn helper(flag bool) i32 {
    if flag {
        return 1;
    } else {
        return 2;
    }
}

fn main() void {
    let total = helper(true);
    print total;
}
`
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	normal := Check(inputs, diagnostics, Config{})
	if !normal.Successful() || normal.IR() == nil {
		t.Fatalf("default-config Check failed: successful=%v diagnostics=%+v", normal.Successful(), diagnostics.Items())
	}

	limitedInputs, limitedDiagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	limited := Check(limitedInputs, limitedDiagnostics, Config{MaxIRNodes: 1})
	if limited.Successful() {
		t.Fatal("MaxIRNodes=1 should fail typed-IR construction and gate the result")
	}
	if limited.IR() != nil {
		t.Fatal("failed typed-IR construction must publish no IR")
	}
}

// TestCheckThreadsConfigIntoBothPhases confirms the public entry point passes
// Config through to run06b (entry validation here), so callers configuring
// the checker through Check get the behavior run06b alone provides.
func TestCheckThreadsConfigIntoBothPhases(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn start() void {}\n")})
	id := entrySymbol(t, inputs, "start")
	result := Check(inputs, diagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: id}})
	if !result.Successful() || result.IR() == nil {
		t.Fatalf("Check with valid EntryRequired failed: successful=%v diagnostics=%+v", result.Successful(), diagnostics.Items())
	}

	badInputs, badDiagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn start() void {}\n")})
	badID := entrySymbol(t, badInputs, "start")
	bad := Check(badInputs, badDiagnostics, Config{Entry: EntryPoint{Mode: EntryRequired, Symbol: badID + 1000}})
	if bad.Successful() || bad.IR() != nil {
		t.Fatalf("Check with an out-of-range EntryRequired symbol succeeded: %+v", badDiagnostics.Items())
	}
}

// TestCheckNilDiagnosticsDoesNotPanic mirrors newGeneration's existing
// nil-DiagnosticSet tolerance so a real caller can invoke Check without first
// constructing a set.
func TestCheckNilDiagnosticsDoesNotPanic(t *testing.T) {
	inputs, _ := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	result := Check(inputs, nil, Config{})
	if !result.Successful() || result.IR() == nil {
		t.Fatalf("Check with nil diagnostics failed: successful=%v", result.Successful())
	}
}
