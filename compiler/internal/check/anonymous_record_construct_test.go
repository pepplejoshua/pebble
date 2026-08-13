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
//
// A later extension (T0510) widened recordFieldDeclaredType's walk-time
// declared-type grounding beyond array-typed fields to PLAIN struct-typed
// fields (NominalStruct, never a tagged union): a NESTED anonymous struct
// literal (`Outer.{ inner = .{ a = 1 } }` — a nested-anonymous-inner form
// that failed for the NAMED outer literal too) had no way to recover its
// destination declaration, because the field's own destination was never
// grounded. That is the case TestNestedAnonymousStructFieldCovers below.

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

// TestOptionalFieldSomePayloadConstruction (Phase 3 #46) proves that a `some
// <payload>` assigned to an OPTIONAL-typed struct field constructs cleanly for
// both the named and anonymous record literal form, in both the matching-type
// literal-payload shape and the width-converting payload shape (a u8 local
// into a ?u32 field). Before the fix, recordFieldGroundable only grounded
// ARRAY- and plain STRUCT-typed fields as KNOWN destinations at walk time, so
// the SomeExpr's optional-type pinning (Phase 3 #27) never fired inside a
// record literal and the field-role compatibility rejected `?<payload's own
// type>` against the field's declared optional type as an unconvertible pair
// (C0601). The `none` value into an optional field and the Phase 3 #27
// plain-local/return/argument positions are pinned as regressions.
func TestOptionalFieldSomePayloadConstruction(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"named-matching-literal", `type Box = struct { opt ?i32; };
fn check() void {
    var b Box = Box.{ opt = some 5 };
}`},
		{"named-width-converting-payload", `type Box = struct { opt ?u32; };
fn check() void {
    var x u8 = 5;
    var b Box = Box.{ opt = some x };
}`},
		{"anonymous-matching-literal", `type Box = struct { opt ?i32; };
fn check() void {
    var b Box = .{ opt = some 5 };
}`},
		{"anonymous-width-converting-payload", `type Box = struct { opt ?u32; };
fn check() void {
    var x u8 = 5;
    var b Box = .{ opt = some x };
}`},
		{"inferred-named-width-converting-payload", `type Box = struct { opt ?u32; };
fn check() void {
    var x u8 = 5;
    var b = Box.{ opt = some x };
}`},
		{"none-value-named", `type Box = struct { opt ?i32; };
fn check() void {
    var b Box = Box.{ opt = none };
}`},
		{"generic-optional-field-typed", `type Box[T] = struct { opt ?T; };
fn check() void {
    var b Box[i32] = Box[i32].{ opt = some 5 };
}`},
		{"generic-optional-field-width-converting", `type Box[T] = struct { opt ?u32; };
fn check() void {
    var x u8 = 5;
    var b Box[i32] = Box[i32].{ opt = some x };
}`},
		{"plain-local-matching-literal", `fn check() void {
    var o ?i32 = some 5;
}`},
		{"plain-local-width-converting-payload", `fn check() void {
    var x u8 = 5;
    var o ?u32 = some x;
}`},
		{"plain-return-width-converting-payload", `fn mk() ?u32 {
    var x u8 = 5;
    return some x;
}`},
		{"plain-argument-width-converting-payload", `fn g(o ?u32) void {}
fn check() void {
    var x u8 = 5;
    g(some x);
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
				t.Fatalf("expected construction to check cleanly, got diagnostics: %+v", diagnostics.Items())
			}
			for _, item := range diagnostics.Items() {
				if item.Code == "C0619" {
					t.Fatalf("leaked the C0619 internal-error path: %+v", diagnostics.Items())
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
// unknown field name, a missing required field, and an optional-typed field
// value whose some-payload form the named form also rejects.
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
		{"optional-some-field", "type Box = struct { opt ?i32; };", `var b Box = Box.{ opt = some "hi" };`, `var b Box = .{ opt = some "hi" };`, "C0601"},
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

// TestNestedAnonymousStructFieldCovers is the T0510 regression suite for a
// nested ANONYMOUS struct literal used as a struct field's value. Before the
// fix, recordFieldDeclaredType grounded only ARRAY-typed fields, so a
// struct-typed field's own destination was never pinned at walk time and the
// inner `.{ ... }` literal (which has no base name to anchor itself) stayed
// an unbound inference variable — producing T0510 for the NAMED outer form
// too. The suite covers the exact repro (named outer, anonymous inner),
// the anonymous-outer sibling, two-level-deep nesting, the named-inner
// regression, the array-field regression, and the tagged-union `.{ Int = 42
// }` construction regression (a separate mechanism that must keep working).
func TestNestedAnonymousStructFieldCovers(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"named-outer-anon-inner", `type Inner = struct { a i32; };
type Outer = struct { inner Inner; };
fn check() void {
    var o Outer = Outer.{ inner = .{ a = 1 } };
}`},
		{"anon-outer-anon-inner", `type Inner = struct { a i32; };
type Outer = struct { inner Inner; };
fn check() void {
    var o Outer = .{ inner = .{ a = 1 } };
}`},
		{"inferred-named-outer", `type Inner = struct { a i32; };
type Outer = struct { inner Inner; };
fn check() void {
    var o = Outer.{ inner = .{ a = 1 } };
}`},
		{"two-level-deep", `type Leaf = struct { v i32; };
type Mid = struct { leaf Leaf; };
type Top = struct { mid Mid; };
fn check() void {
    var t Top = Top.{ mid = .{ leaf = .{ v = 7 } } };
}`},
		{"named-inner-nested", `type Inner = struct { a i32; };
type Outer = struct { inner Inner; };
fn check() void {
    var o Outer = Outer.{ inner = Inner.{ a = 1 } };
}`},
		{"array-field", `type Box = struct { data [3]i32; };
fn check() void {
    var b Box = .{ data = [1, 2, 3] };
}`},
		{"tagged-union-construct", `type Choice = union enum { Int int; Float f64; };
fn check() void {
    var c Choice = Choice.{ Int = 42 };
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
				t.Fatalf("expected clean check, got diagnostics: %+v", diagnostics.Items())
			}
			for _, item := range diagnostics.Items() {
				if item.Code == "C0619" {
					t.Fatalf("leaked the C0619 internal-error path: %+v", diagnostics.Items())
				}
			}
		})
	}
}
