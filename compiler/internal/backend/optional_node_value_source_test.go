package backend

import "testing"

// Phase 3 #14 — `none` and `some value` optional nodes in value-source
// positions. The Optional `?T` row (tracker 14 line 137) closed the
// PAYLOAD-TYPE gaps (array/slice payloads, 08ce755); this row's distinct
// remaining gap turned out to be value-source + payload-typedef: a `none`
// (or a `some` whose enum/union payload is otherwise unreferenced as a value)
// constructs an enum- or union-payload optional without ever carrying the
// payload as a value — a NoneOptional node holds only the optional type, so
// collectEnumTypes/collectUnionTypes never discovered the payload enum's C
// typedef and the emitted C referenced an undefined pebble_enum_<typeID>_t.
// Fixed by mirroring collectStructTypes' optional-payload scan into both
// collectors. The second half of this file proves the previously-unproven
// `some`-wraps-a-non-trivial-expression shapes (a call result, a field read,
// a tuple element) now compile-link-run in every position.

// TestNoneEnumPayloadShapes proves `none` (and `some`/reassignment) for a
// plain-enum-payload optional in every position when the enum is otherwise
// unreferenced — the exact repro of the typedef-collection gap. Before the
// fix each case emitted C naming an undefined pebble_enum_<typeID>_t; now
// each compiles, runs, and reports the expected presence/value.
func TestNoneEnumPayloadShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-none", "type Color = enum { red, green, blue }; fn main() int { var o ?Color = none; if o.has_value { return 1; } return 0; }", 0},
		{"local-some-unwrap", "type Color = enum { red, green, blue }; fn main() int { var o ?Color = some Color.green; if o! == Color.green { return 42; } return 0; }", 42},
		{"call-argument-none", "type Color = enum { red, green, blue }; fn g(o ?Color) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", 0},
		{"call-argument-some", "type Color = enum { red, green, blue }; fn g(o ?Color) int { if o.has_value { if o! == Color.blue { return 42; } } return 0; } fn main() int { return g(some Color.blue); }", 42},
		{"return-none", "type Color = enum { red, green, blue }; fn mk() ?Color { return none; } fn main() int { var o ?Color = mk(); if o.has_value { return 1; } return 0; }", 0},
		{"return-some", "type Color = enum { red, green, blue }; fn mk() ?Color { return some Color.green; } fn main() int { var o ?Color = mk(); if o.has_value { if o! == Color.green { return 42; } } return 0; }", 42},
		{"struct-field-none", "type Color = enum { red, green, blue }; type Box = struct { value ?Color; }; fn main() int { var b Box = Box.{ value = none }; if b.value.has_value { return 1; } return 0; }", 0},
		{"struct-field-some", "type Color = enum { red, green, blue }; type Box = struct { value ?Color; }; fn main() int { var b Box = Box.{ value = some Color.green }; if b.value.has_value { return 42; } return 0; }", 42},
		{"reassign-none-to-some", "type Color = enum { red, green, blue }; fn main() int { var o ?Color = none; o = some Color.green; if o.has_value { return 42; } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestNoneTaggedUnionPayloadShapes is the tagged-union mirror of the enum
// cases: a union-payload optional constructed with `none` (or referenced only
// as an optional parameter) never constructs the union as a value, so without
// collectUnionTypes' optional-payload scan neither the union's tag enum nor
// its tagged struct typedef was emitted and cc failed on an undefined
// pebble_enum_<typeID>_t.
func TestNoneTaggedUnionPayloadShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-none", "type Choice = union enum { empty void; value i32; }; fn main() int { var o ?Choice = none; if o.has_value { return 1; } return 0; }", 0},
		{"call-argument-none", "type Choice = union enum { empty void; value i32; }; fn g(o ?Choice) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", 0},
		{"struct-field-none", "type Choice = union enum { empty void; value i32; }; type Box = struct { value ?Choice; }; fn main() int { var b Box = Box.{ value = none }; if b.value.has_value { return 1; } return 0; }", 0},
		{"local-some-round-trips", "type Choice = union enum { empty void; value i32; }; fn main() int { var o ?Choice = some Choice.value(5); if o.has_value { switch o! { case Choice.value: return 42; case Choice.empty: return 1; } } return 0; }", 42},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSomeWrappingCallResult proves `some <call-result>` — a SomeOptional
// whose payload is a helper call returning the payload type — in a local
// declaration, a call argument, a return value, and a struct-field
// construction value. The payload is built by the grammar its own type
// selects (a DirectCall of the payload type), the same path any
// non-literal some payload takes.
func TestSomeWrappingCallResult(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"local-init", "fn pick() int { return 7; } fn main() int { var o ?int = some pick(); if o.has_value { return o!; } return 0; }", 7},
		{"call-argument", "fn pick() int { return 7; } fn g(o ?int) int { if o.has_value { return o!; } return 0; } fn main() int { return g(some pick()); }", 7},
		{"return", "fn pick() int { return 7; } fn mk() ?int { return some pick(); } fn main() int { var o ?int = mk(); if o.has_value { return o!; } return 0; }", 7},
		{"struct-field", "fn pick() int { return 7; } type Box = struct { value ?int; }; fn main() int { var b Box = Box.{ value = some pick() }; if b.value.has_value { return b.value!; } return 0; }", 7},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

// TestSomeWrappingFieldRead proves `some <struct-field-read>` in the same four
// positions, plus `some <tuple-element>` in a local/argument/return — the
// payload is a Load of a FieldPlace / tuple ordinal, again delegated to the
// payload type's own read grammar.
func TestSomeWrappingFieldRead(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"struct-field-local-init", "type P = struct { x int; }; fn main() int { var s P = P.{ x = 7 }; var o ?int = some s.x; if o.has_value { return o!; } return 0; }", 7},
		{"struct-field-argument", "type P = struct { x int; }; fn g(o ?int) int { if o.has_value { return o!; } return 0; } fn main() int { var s P = P.{ x = 7 }; return g(some s.x); }", 7},
		{"struct-field-return", "type P = struct { x int; }; fn mk(s P) ?int { return some s.x; } fn main() int { var s P = P.{ x = 7 }; var o ?int = mk(s); if o.has_value { return o!; } return 0; }", 7},
		{"struct-field-struct-field", "type P = struct { x int; }; type Box = struct { value ?int; }; fn main() int { var s P = P.{ x = 7 }; var b Box = Box.{ value = some s.x }; if b.value.has_value { return b.value!; } return 0; }", 7},
		{"tuple-element-local-init", "fn main() int { var t (int, int) = (5, 7); var o ?int = some t.1; if o.has_value { return o!; } return 0; }", 7},
		{"tuple-element-argument", "fn g(o ?int) int { if o.has_value { return o!; } return 0; } fn main() int { var t (int, int) = (5, 7); return g(some t.1); }", 7},
		{"tuple-element-return", "fn mk(t (int, int)) ?int { return some t.1; } fn main() int { var t (int, int) = (5, 7); var o ?int = mk(t); if o.has_value { return o!; } return 0; }", 7},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}
