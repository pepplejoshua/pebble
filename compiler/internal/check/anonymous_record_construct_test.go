package check

import (
	"fmt"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
)

// Anonymous `.{ ... }` struct construction (Phase 3 #33). The resolver only
// resolves record-literal field names when the literal carries a base-type
// name (Point.{ x = 1 }); the anonymous form (.{ x = 1 }) has no base-name
// node, so field.Member stayed 0. That broke two downstream steps:
//
//   - walk-time declared-type grounding (recordFieldDeclaredType) keyed off
//     field.Member, so an array-typed field value's element type was never
//     pinned early and the array literal stayed its default-width type
//     (`.{ data = [1, 2, 3] }` into a [3]i32 field failed C0601);
//   - buildRecordConstruct's IR-build step emitted FieldInit{Field: 0},
//     tripping the C0619 internal-error catch-all.
//
// Fixed by resolving field.Member for the anonymous form at walk time in
// prepareRecord — re-deriving it by name against the destination struct
// declaration (the same declaration recordReceiverTerm grounds from the
// contextual expected type) — with buildRecordConstruct's existing
// memberSymbol-by-name fallback (the one buildTaggedVariantConstruct already
// used for `.{ Int = 42 }`) as the post-solve closing net.

// TestAnonymousRecordConstructKnownDestinationPositions proves the anonymous
// form checks successfully (no C0619, no diagnostics) in every position
// where the destination struct type is known from context, including
// aggregate-shaped field values that share the array literal's need for
// early declared-type grounding.
func TestAnonymousRecordConstructKnownDestinationPositions(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"local-decl", `type Point = struct { x i32; y i32; };
fn check() void {
    var p Point = .{ x = 1, y = 2 };
}`},
		{"param", `type Point = struct { x i32; y i32; };
fn take(p Point) void {}
fn check() void {
    take(.{ x = 1, y = 2 });
}`},
		{"return", `type Point = struct { x i32; y i32; };
fn make() Point { return .{ x = 1, y = 2 }; }`},
		{"reassign", `type Point = struct { x i32; y i32; };
fn check() void {
    var p Point = Point.{ x = 1, y = 2 };
    p = .{ x = 3, y = 4 };
}`},
		{"array-field", `type Box = struct { data [3]i32; };
fn check() void {
    var b Box = .{ data = [1, 2, 3] };
}`},
		{"nested-named-struct-field", `type Inner = struct { a i32; };
type Outer = struct { inner Inner; };
fn check() void {
    var o Outer = .{ inner = Inner.{ a = 1 } };
}`},
		{"optional-none-field", `type Box = struct { opt ?i32; };
fn check() void {
    var b Box = .{ opt = none };
}`},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(test.source)})
			handoff := run06a(inputs, diagnostics, Config{})
			if handoff == nil {
				t.Fatalf("06a failed: %+v", diagnostics.Items())
			}
			result := run06b(handoff, diagnostics, Config{}, inputs.Types)
			if !result.Successful() {
				t.Fatalf("expected anonymous construction to check cleanly, got diagnostics: %+v", diagnostics.Items())
			}
			for _, item := range diagnostics.Items() {
				if item.Code == "C0619" {
					t.Fatalf("anonymous construction leaked the C0619 internal-error path: %+v", diagnostics.Items())
				}
			}
		})
	}
}

// TestAnonymousRecordConstructNoDestinationRejected proves that when the
// destination type cannot be inferred at all, the anonymous form produces a
// clean inference diagnostic (T0510) rather than the C0619 internal-error
// catch-all.
func TestAnonymousRecordConstructNoDestinationRejected(t *testing.T) {
	source := `type Point = struct { x i32; y i32; };
fn check() void {
    let p = .{ x = 1, y = 2 };
}`
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatalf("06a failed: %+v", diagnostics.Items())
	}
	result := run06b(handoff, diagnostics, Config{}, inputs.Types)
	if result.Successful() {
		t.Fatalf("expected rejection for an anonymous construction with no inferable destination")
	}
	for _, item := range diagnostics.Items() {
		if item.Code == "C0619" {
			t.Fatalf("no-destination anonymous construction leaked the C0619 internal-error path: %+v", diagnostics.Items())
		}
	}
}

// TestAnonymousRecordConstructMatchesNamedDiagnostics proves the anonymous
// form produces the same class of field-level diagnostic the named form
// already produces for equivalent mistakes: a wrong field value type, an
// unknown field name, a missing required field, an optional-typed field
// value whose some-payload form the named form also rejects, and a nested
// anonymous struct literal (a pre-existing gap that fails for the named
// outer form too).
func TestAnonymousRecordConstructMatchesNamedDiagnostics(t *testing.T) {
	cases := []struct {
		name     string
		types    string
		named    string
		anon     string
		wantCode diagnostic.Code
	}{
		{"wrong-type", "type Point = struct { x i32; y i32; };", `var p Point = Point.{ x = "str", y = 2 };`, `var p Point = .{ x = "str", y = 2 };`, "T0505"},
		{"unknown-field", "type Point = struct { x i32; y i32; };", `var p Point = Point.{ z = 1 };`, `var p Point = .{ z = 1 };`, "T0507"},
		{"missing-field", "type Point = struct { x i32; y i32; };", `var p Point = Point.{ x = 1 };`, `var p Point = .{ x = 1 };`, "C0605"},
		{"optional-some-field", "type Box = struct { opt ?i32; };", `var b Box = Box.{ opt = some 5 };`, `var b Box = .{ opt = some 5 };`, "C0601"},
		{"nested-anon-struct-field", "type Inner = struct { a i32; };\ntype Outer = struct { inner Inner; };", `var o Outer = Outer.{ inner = .{ a = 1 } };`, `var o Outer = .{ inner = .{ a = 1 } };`, "T0510"},
	}
	for _, test := range cases {
		t.Run(test.name, func(t *testing.T) {
			named := fmt.Sprintf("%s\nfn check() void {\n    %s\n}", test.types, test.named)
			inputs, diags := factInputs(t, checkProvider{"main.peb": []byte(named)})
			handoff := run06a(inputs, diags, Config{})
			if handoff == nil {
				t.Fatalf("named form 06a failed: %+v", diags.Items())
			}
			namedResult := run06b(handoff, diags, Config{}, inputs.Types)
			if namedResult.Successful() {
				t.Fatalf("expected the named form to be rejected for %s", test.name)
			}
			if !hasCode(diags, test.wantCode) {
				t.Fatalf("named form: expected diagnostic code %s, got %+v", test.wantCode, diags.Items())
			}

			anon := fmt.Sprintf("%s\nfn check() void {\n    %s\n}", test.types, test.anon)
			inputs, diags = factInputs(t, checkProvider{"main.peb": []byte(anon)})
			handoff = run06a(inputs, diags, Config{})
			if handoff == nil {
				t.Fatalf("anonymous form 06a failed: %+v", diags.Items())
			}
			anonResult := run06b(handoff, diags, Config{}, inputs.Types)
			if anonResult.Successful() {
				t.Fatalf("expected the anonymous form to be rejected for %s", test.name)
			}
			if !hasCode(diags, test.wantCode) {
				t.Fatalf("anonymous form: expected diagnostic code %s, got %+v", test.wantCode, diags.Items())
			}
			for _, item := range diags.Items() {
				if item.Code == "C0619" {
					t.Fatalf("anonymous form leaked the C0619 internal-error path for %s: %+v", test.name, diags.Items())
				}
			}
		})
	}
}
