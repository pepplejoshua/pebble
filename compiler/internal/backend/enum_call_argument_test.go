package backend

import "testing"

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
