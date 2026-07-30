package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

func validateSwitchesFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	if handoff.GenerationHadErrors {
		t.Fatalf("06a reported errors: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, validateSwitches(handoff, records, diagnostics, Config{})
}

func TestValidateSwitchesEnumExhaustiveNoElse(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
type Color = enum { red, blue };
fn classify(color Color) i32 {
    switch color {
    case Color.red: return 1;
    case Color.blue: return 2;
    }
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("exhaustive enum switch without else was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesEnumMissingVariantWithElse(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
type Color = enum { red, blue, green };
fn classify(color Color) i32 {
    switch color {
    case Color.red: return 1;
    case Color.blue: return 2;
    else: return 0;
    }
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("enum switch with missing variant and else was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesEnumMissingVariantNoElse(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
type Color = enum { red, blue, green };
fn classify(color Color) i32 {
    switch color {
    case Color.red: return 1;
    case Color.blue: return 2;
    }
}
`)
	// Nonexhaustive without else is NOT a hard rejection from validateSwitches;
	// it only affects exit-set fallthrough computation (handled by
	// validateControlFlow / switchIsExhaustive). The C0607 for a non-void
	// function comes from validateControlFlow's missing-return check.
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("nonexhaustive enum switch without else should not be a switch validation error: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesEnumDuplicateVariant(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
type Color = enum { red, blue };
fn classify(color Color) i32 {
    switch color {
    case Color.red, Color.red: return 1;
    case Color.blue: return 2;
    }
}
`)
	if valid || !hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("duplicate enum variant was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesBoolExhaustiveNoElse(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
fn classify(flag bool) i32 {
    switch flag {
    case true: return 1;
    case false: return 0;
    }
}
`)
	if !valid || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("exhaustive bool switch without else was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesScalarWithElse(t *testing.T) {
	for name, source := range map[string]string{
		"i32": `
fn classify(x i32) void {
    switch x {
    case 1: print 1;
    case 2: print 2;
    else: print 0;
    }
}
`,
		"char": `
fn classify(c char) void {
    switch c {
    case 'a': print 1;
    case 'b': print 2;
    else: print 0;
    }
}
`,
		"str": `
fn classify(s str) void {
    switch s {
    case "hello": print 1;
    case "world": print 2;
    else: print 0;
    }
}
`,
	} {
		t.Run(name, func(t *testing.T) {
			diagnostics, valid := validateSwitchesFixture(t, source)
			if !valid || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
				t.Fatalf("scalar switch with else was rejected: %+v", diagnostics.Items())
			}
		})
	}
}

func TestValidateSwitchesScalarDuplicateConstant(t *testing.T) {
	diagnostics, valid := validateSwitchesFixture(t, `
fn classify(x i32) void {
    switch x {
    case 1, 1: print 1;
    case 2: print 2;
    else: print 0;
    }
}
`)
	if valid || !hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("duplicate scalar constant was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateSwitchesInvalidCategory(t *testing.T) {
	tests := []struct {
		name   string
		source string
	}{
		{
			name: "struct",
			source: `
type Point = struct { x i32; y i32; };
fn test(p Point) void {
    switch p { else: print 0; }
}
`,
		},
		{
			name: "pointer",
			source: `
fn test(p *i32) void {
    switch p { else: print 0; }
}
`,
		},
		{
			name: "float",
			source: `
fn test(f f32) void {
    switch f { else: print 0; }
}
`,
		},
		{
			name: "array",
			source: `
fn test(a [3]i32) void {
    switch a { else: print 0; }
}
`,
		},
		{
			name: "slice",
			source: `
fn test(s []i32) void {
    switch s { else: print 0; }
}
`,
		},
	}
	for _, tc := range tests {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, valid := validateSwitchesFixture(t, tc.source)
			if valid || !hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
				t.Fatalf("invalid category switch was not rejected (valid=%v): %+v", valid, diagnostics.Items())
			}
		})
	}
}

func TestValidateSwitchesInactiveGuarded(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(`
type Color = enum { red, blue };
fn classify(color Color) void {
    switch color {
    case Color.red: print 1;
    case Color.blue: print 2;
    }
}
`)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	if handoff.GenerationHadErrors {
		t.Fatalf("06a reported errors: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}

	// Mark the switch record as guarded with a non-matching choice.
	for index := range handoff.Records.values {
		if handoff.Records.values[index].Control != nil && handoff.Records.values[index].Control.Kind == controlSwitch {
			handoff.Records.values[index].Header.Alternative = alternativeTag{Guarded: true, Choice: 999999, Index: 0}
			break
		}
	}

	diagnostics = diagnostic.NewDiagnosticSet()
	if !validateSwitches(handoff, records, diagnostics, Config{}) || hasControlDiagnostic(diagnostics, CodeInvalidTarget) {
		t.Fatalf("inactive guarded switch produced diagnostics: %+v", diagnostics.Items())
	}
}
