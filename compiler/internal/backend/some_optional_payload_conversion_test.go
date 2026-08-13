package backend

import (
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/check"
	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// assertCheckRejects builds the same fixture buildFixture builds but asserts
// the CHECKER itself rejects the source with at least one diagnostic whose
// message contains wantSubstring. The payload-conversion rejection cases below
// are checker rejections (a compatibleForbidden optional-injection
// compatibility or a literal-fit failure), which surface before Emit, so
// emitAndRunRejects (which requires a successful check) cannot assert them.
func assertCheckRejects(t *testing.T, sourceText, wantSubstring string) {
	t.Helper()
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "facts"}, fixtureProvider{"main.peb": []byte(sourceText)}, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if result.Successful() {
		t.Fatalf("check succeeded, want rejection containing %q", wantSubstring)
	}
	for _, item := range diagnostics.Items() {
		if strings.Contains(item.Message, wantSubstring) {
			return
		}
	}
	t.Fatalf("check failed but no diagnostic contains %q: %+v", wantSubstring, diagnostics.Items())
}

// Phase 3 #27 — `some S` to optional `T` with PAYLOAD CONVERSION. The
// Optional `?T` row's sibling rows closed the payload-TYPE gaps (array/slice
// payloads, 08ce755) and the none/some VALUE-SOURCE and payload-TYPEDEF gaps
// (ffb365b); the distinct gap this file covers is a `some <value>` whose
// payload's OWN type needs a width/type conversion to match the optional's
// declared payload type — `some x` where x is a u8 local (or a narrower call
// result / field read) into a `?u32`/`?i64`/... destination. The literal half
// of the row ("implicit for a literal `some`") already worked (an int literal
// fits any destination via literal-fit), but every non-literal payload needing
// a conversion was checker-rejected with C0601: the SomeExpr typed itself as
// `?<payload's own type>` (ConstrainShape OptionalShape(child.Term)), so the
// binding/return/argument compatibility classified `?u8`->`?u32` as
// compatibleForbidden even though the plain, non-optional u8->u32 conversion
// is accepted. Fixed by (a) pinning the SomeExpr's optional type to the known
// destination's optional type at solve, and (b) wrapping the payload child in
// the ordinary coercion node (IntegerCast, FloatCast, CharToInteger, ...) at
// IR-build when its own type differs from the SomeOptional's payload type —
// mirroring the expressionTuple case's element-coercion machinery exactly.

// TestSomePayloadWidthConversion compiles, links, and runs `some <narrower
// local / call result / field read>` constructed into a WIDER optional payload
// type, unwrapping and checking the round-tripped value in every position.
// The payload conversion is a genuine IntegerCast (not a C-level slop): the
// 300-in-u16-into-?u32 case would truncate to 44 if the payload were silently
// kept at u8.
func TestSomePayloadWidthConversion(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"u8-local-into-u32", "fn main() int { var x u8 = 5; var o ?u32 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"u8-local-into-u64", "fn main() int { var x u8 = 5; var o ?u64 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"u8-local-into-int", "fn main() int { var x u8 = 5; var o ?int = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"i8-local-into-i64", "fn main() int { var x i8 = 5; var o ?i64 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"u16-local-into-u32", "fn main() int { var x u16 = 5; var o ?u32 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"i32-local-into-i64", "fn main() int { var x i32 = 5; var o ?i64 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"u32-local-into-u64", "fn main() int { var x u32 = 5; var o ?u64 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"u16-local-300-into-u32", "fn main() int { var x u16 = 300; var o ?u32 = some x; if o.has_value { if o! == 300 { return 42; } } return 0; }", 42},
		{"u8-call-result-into-u32", "fn pick() u8 { return 5; } fn main() int { var o ?u32 = some pick(); if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"i32-call-result-into-i64", "fn pick() i32 { return 7; } fn main() int { var o ?i64 = some pick(); if o.has_value { if o! == 7 { return 42; } } return 0; }", 42},
		{"u32-call-result-into-u64", "fn pick() u32 { return 7; } fn main() int { var o ?u64 = some pick(); if o.has_value { if o! == 7 { return 42; } } return 0; }", 42},
		{"i32-field-read-into-i64", "type P = struct { x i32; }; fn main() int { var s P = P.{ x = 7 }; var o ?i64 = some s.x; if o.has_value { if o! == 7 { return 42; } } return 0; }", 42},
		{"u16-field-read-into-i64", "type P = struct { x u16; }; fn main() int { var s P = P.{ x = 7 }; var o ?i64 = some s.x; if o.has_value { if o! == 7 { return 42; } } return 0; }", 42},
		{"f32-local-into-f64", "fn main() int { var f f32 = 2.5; var o ?f64 = some f; if o.has_value { if o! == 2.5 { return 42; } } return 0; }", 42},
		{"char-local-into-i32", "fn main() int { var c char = 'a'; var o ?i32 = some c; if o.has_value { if o! == 97 { return 42; } } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSomePayloadConversionPositions proves the same payload conversion in
// every value-source position beyond a local declaration: a call argument, a
// return value, and a reassignment (each with a payload whose own type needs
// the conversion).
func TestSomePayloadConversionPositions(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"argument-u8-call-into-u32", "fn pick() u8 { return 5; } fn g(o ?u32) int { if o.has_value { if o! == 5 { return 42; } } return 0; } fn main() int { return g(some pick()); }", 42},
		{"argument-u8-local-into-u32", "fn g(o ?u32) int { if o.has_value { if o! == 5 { return 42; } } return 0; } fn main() int { var x u8 = 5; return g(some x); }", 42},
		{"return-u8-local-into-u32", "fn mk() ?u32 { var x u8 = 5; return some x; } fn main() int { var o ?u32 = mk(); if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"return-field-read-into-u32", "type P = struct { x u8; }; fn mk(s P) ?u32 { return some s.x; } fn main() int { var s P = P.{ x = 5 }; var o ?u32 = mk(s); if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"reassign-u8-into-u32", "fn main() int { var x u8 = 5; var o ?u32 = none; o = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSomeLiteralPayloadFits pins the row's already-working literal half so
// the conversion fix does not regress it: an int literal fits every integer
// optional payload and a float literal fits both float payloads.
func TestSomeLiteralPayloadFits(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"int-literal-i32", "fn main() int { var o ?i32 = some 5; if o.has_value { return o!; } return 0; }", 5},
		{"int-literal-i64", "fn main() int { var o ?i64 = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-u8", "fn main() int { var o ?u8 = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-u16", "fn main() int { var o ?u16 = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-u32", "fn main() int { var o ?u32 = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-u64", "fn main() int { var o ?u64 = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-uint", "fn main() int { var o ?uint = some 5; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
		{"int-literal-int", "fn main() int { var o ?int = some 5; if o.has_value { return o!; } return 0; }", 5},
		{"float-literal-f32", "fn main() int { var o ?f32 = some 2.5; if o.has_value { if o! == 2.5 { return 42; } } return 0; }", 42},
		{"float-literal-f64", "fn main() int { var o ?f64 = some 2.5; if o.has_value { if o! == 2.5 { return 42; } } return 0; }", 42},
		{"matching-payload-still-works", "fn main() int { var x u8 = 5; var o ?u8 = some x; if o.has_value { if o! == 5 { return 42; } } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSomePayloadConversionRejects pins the conversions that must STAY
// rejected: a payload with no conversion path to the destination payload (str,
// struct), and a literal that does not fit the destination payload (a
// float literal into an integer optional, an out-of-range int literal). The
// rejection surfaces as the payload's own optional-injection compatibility
// failure (C0601) or the literal-fit failure (T0508).
func TestSomePayloadConversionRejects(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   string
	}{
		{"str-payload-into-i32", "fn main() int { var o ?i32 = some \"hi\"; if o.has_value { return o!; } return 0; }", "cannot convert"},
		{"struct-payload-into-i32", "type P = struct { x i32; }; fn main() int { var s P = P.{ x = 5 }; var o ?i32 = some s; return 0; }", "cannot convert"},
		{"float-literal-into-i64", "fn main() int { var o ?i64 = some 2.5; if o.has_value { return o! as int; } return 0; }", "does not fit"},
		{"large-literal-into-u8", "fn main() int { var o ?u8 = some 300; if o.has_value { return o! as int; } return 0; }", "does not fit"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			assertCheckRejects(t, tc.source, tc.want)
		})
	}
}
