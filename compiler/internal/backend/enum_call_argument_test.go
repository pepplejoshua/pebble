package backend

import (
	"bytes"
	"strings"
	"testing"
)

// Phase 3 #13 — an enum variant literal as a direct call argument. A
// plain-enum parameter previously accepted only a reference to an in-scope
// enum-typed local of exactly the parameter's type (a SymbolValue) — passing
// the literal directly (`check(Color.green)`) was rejected at Emit with "want
// a reference to an enum-typed local of exactly that type in scope (binding
// the value into a local first is required)". The plain-enum branch of
// buildCallArgument (calls.go) now delegates an EnumVariantValue /
// payload-less VariantConstruct argument to buildEnumValue's variant-literal
// case, the same grammar an enum-typed local's declaration uses, which emits
// the variant's own C enum constant — trivially valid at the parameter's own
// pebble_enum_<typeID>_t typedef, the exact same constant the workaround
// (binding into a local first) emits.

// TestEnumVariantLiteralAsCallArgument proves the exact reported repro: a
// variant literal passed directly as a plain-enum call argument, from the
// entry to a helper. Before the fix this failed at Emit; now the helper
// receives the variant's constant and returns 11.
func TestEnumVariantLiteralAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { green, red, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { return check(Color.green); }", false, 11, false)
}

// TestEnumVariantCallFormAsCallArgument proves the zero-payload
// parenthesized-call form of a plain enum's variant (`Color.green()`), which
// lowers to a VariantConstruct — the same literal family, routed through the
// same widened case.
func TestEnumVariantCallFormAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { green, red, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { return check(Color.green()); }", false, 11, false)
}

// TestEnumVariantLiteralAsCallArgumentEveryVariant proves all three variants
// of a three-variant enum survive the direct-argument lowering, so the
// variant's ordinal constant (not a hard-coded green) is what reaches the
// helper.
func TestEnumVariantLiteralAsCallArgumentEveryVariant(t *testing.T) {
	cases := []struct {
		variant string
		want    int
	}{
		{"Color.red", 1},
		{"Color.green", 2},
		{"Color.blue", 3},
	}
	for _, tc := range cases {
		t.Run(tc.variant, func(t *testing.T) {
			emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { switch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; } } fn main() int { return check("+tc.variant+"); }", false, tc.want, false)
		})
	}
}

// TestEnumVariantLiteralAsArgumentFromGenericContext proves the shape from
// inside a generic context — the "generic-self" family: a generic helper
// whose body passes a concrete enum's variant literal to a plain-enum
// parameter, and a generic function called with a variant literal at the
// entry's call site. Both route through the same widened plain-enum argument
// branch (the enum here is concrete; the surrounding function is generic).
func TestEnumVariantLiteralAsArgumentFromGenericContext(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { switch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; } } fn call_through[T]() int { return check(Color.green); } fn main() int { return call_through[i32](); }", false, 2, false)
	emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { switch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; } } fn call_through[T](c Color) int { return check(c); } fn main() int { return call_through[i32](Color.blue); }", false, 3, false)
}

// TestEnumLocalBindingAsCallArgumentStillWorks is the workaround regression
// guard: binding the variant into an enum-typed local first and passing the
// local (a SymbolValue) must keep working exactly as before the fix.
func TestEnumLocalBindingAsCallArgumentStillWorks(t *testing.T) {
	emitAndRun(t, "type Color = enum { green, red, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { var c Color = Color.green; return check(c); }", false, 11, false)
}

// --- Phase 3 #35: the remaining plain-enum call-argument value-source shapes ---
// A plain-enum parameter previously accepted only an in-scope enum-typed local
// (a SymbolValue), a variant literal, or a pointer-deref read. The Phase 3 #35
// widening delegates three MORE enum-value shapes to buildEnumValue — a call to
// an enum-returning helper used directly (`check(pick())`, a DirectCall), an
// enum-typed struct field read used directly (`check(s.c)`, a Load of a
// FieldPlace), and an integer-to-enum cast used directly (`check(1 as Color)`,
// a CheckedIntegerToEnum) — emitting the enum's own pebble_enum_<typeID>_t C
// value, trivially valid at the parameter's own typedef. (An IndirectCall whose
// result is an enum is unreachable from real source: a function-typed value's
// signature admits only the result shapes the indirect-call lowering can emit,
// and an enum is not one.)

// TestEnumCallResultAsCallArgument proves the exact call-result repro: the
// result of an enum-returning helper passed directly as a plain-enum call
// argument. Before the fix the DirectCall argument was rejected at Emit with
// "want a reference to an enum-typed local of exactly that type in scope";
// now the helper's returned enum value reaches check, which returns 11.
func TestEnumCallResultAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn pick() Color { return Color.green; } fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { return check(pick()); }", false, 11, false)
}

// TestEnumCallResultAsCallArgumentEveryVariant proves all three variants of a
// three-variant enum survive the direct call-result argument lowering, so the
// actual variant ordinal (not a hard-coded green) is what reaches the helper.
func TestEnumCallResultAsCallArgumentEveryVariant(t *testing.T) {
	cases := []struct {
		variant string
		want    int
	}{
		{"Color.red", 1},
		{"Color.green", 2},
		{"Color.blue", 3},
	}
	for _, tc := range cases {
		t.Run(tc.variant, func(t *testing.T) {
			emitAndRun(t, "type Color = enum { red, green, blue }; fn pick(c Color) Color { return c; } fn check(c Color) int { switch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; } } fn main() int { return check(pick("+tc.variant+")); }", false, tc.want, false)
		})
	}
}

// TestEnumCallResultForwardedAsCallArgument is a nested variant: the call
// result is forwarded through another enum-returning helper before being
// passed directly — the enum value flows through two call expressions, both
// lowered inline at the single argument position.
func TestEnumCallResultForwardedAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn pick() Color { return Color.blue; } fn fwd(c Color) Color => c; fn check(c Color) int { if c == Color.blue { return 11; } return 0; } fn main() int { return check(fwd(pick())); }", false, 11, false)
}

// TestEnumStructFieldAsCallArgument proves the exact struct-field repro: an
// enum-typed struct field read directly as a plain-enum call argument. Before
// the fix the Load-of-FieldPlace argument was rejected at Emit with the same
// "want a reference to an enum-typed local" message; now the field's enum value
// reaches check, which returns 11.
func TestEnumStructFieldAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; type S = struct { c Color; }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { var s S = S.{ c = Color.green }; return check(s.c); }", false, 11, false)
}

// TestEnumStructFieldOfCallResultAsCallArgument is a nested variant: the field
// is read off a struct that is itself a call result (`check(mk().c)`), which
// the checker lowers to a FieldValue (a field read off a non-addressable
// struct value) rather than a Load of a FieldPlace — the same dispatch widening
// covers it through buildEnumValue's FieldValue case.
func TestEnumStructFieldOfCallResultAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; type S = struct { c Color; }; fn mk() S { return S.{ c = Color.blue }; } fn check(c Color) int { if c == Color.blue { return 11; } return 0; } fn main() int { return check(mk().c); }", false, 11, false)
}

// TestEnumStructFieldAsCallArgumentEveryVariant proves each variant ordinal
// survives the struct-field argument lowering, mirroring the literal and
// call-result tables.
func TestEnumStructFieldAsCallArgumentEveryVariant(t *testing.T) {
	cases := []struct {
		field string
		want  int
	}{
		{"Color.red", 1},
		{"Color.green", 2},
		{"Color.blue", 3},
	}
	for _, tc := range cases {
		t.Run(tc.field, func(t *testing.T) {
			emitAndRun(t, "type Color = enum { red, green, blue }; type S = struct { c Color; }; fn mk(c Color) S { return S.{ c = c }; } fn check(c Color) int { switch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; } } fn main() int { var s S = mk("+tc.field+"); return check(s.c); }", false, tc.want, false)
		})
	}
}

// TestEnumIntToEnumCastAsCallArgument proves the exact cast repro: an
// integer-to-enum cast used directly as a plain-enum call argument. Before the
// fix the CheckedIntegerToEnum argument was rejected at Emit with the same
// "want a reference to an enum-typed local" message; now the bounds-checked
// cast value (ordinal 1, green) reaches check, which returns 11.
func TestEnumIntToEnumCastAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { return check(1 as Color); }", false, 11, false)
}

// TestEnumIntToEnumCastFromLocalAsCallArgument is a nested variant: the cast's
// integer source is a local read rather than a literal — `check(n as Color)`
// where n is an in-scope int local.
func TestEnumIntToEnumCastFromLocalAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { var n int = 1; return check(n as Color); }", false, 11, false)
}

// TestEnumIntToEnumCastForwardedAsCallArgument is a nested variant: the cast
// value is forwarded through an enum-returning helper before being passed
// directly — the CheckedIntegerToEnum and DirectCall enum-value shapes combine
// at one argument position.
func TestEnumIntToEnumCastForwardedAsCallArgument(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn fwd(c Color) Color => c; fn check(c Color) int { if c == Color.blue { return 11; } return 0; } fn main() int { return check(fwd(2 as Color)); }", false, 11, false)
}

// TestEnumOutOfRangeCastAsCallArgumentSafePanics proves the cast argument is a
// genuine bounds-checked lowering, not a shortcut: 99 names no real variant of
// a three-variant enum, so in SAFE mode the checked primitive panics before the
// argument is ever used, terminating the process abnormally.
func TestEnumOutOfRangeCastAsCallArgumentSafePanics(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { return check(99 as Color); }", false, 0, true)
}

// TestEnumCallResultAndFieldAndCastArgumentsWritesC pins the emitted C for all
// three widened argument shapes at once: the DirectCall argument is the callee
// invocation itself (`pebble_fn_<callee>(ctx, ...)`, whose return type is the
// enum's own pebble_enum_<typeID>_t), the struct-field argument is the field
// projection (`pebble_local_<sym>.pebble_field_<m>`), and the cast argument is
// the checked runtime call
// (`(pebble_enum_<typeID>_t)pebble_rt_checked_int_to_enum(...)`).
func TestEnumCallResultAndFieldAndCastArgumentsWritesC(t *testing.T) {
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type Color = enum { red, green, blue }; type S = struct { c Color; }; fn pick() Color { return Color.green; } fn check(c Color) int { if c == Color.green { return 11; } return 0; } fn main() int { var s S = S.{ c = Color.green }; return check(pick()) + check(s.c) + check(1 as Color); }")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"pebble_fn_",
		"pebble_field_",
		"(" + enumTypeName(enumType) + ")pebble_rt_checked_int_to_enum((int64_t)(1LL), 3, (PebbleSourceLoc)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}
