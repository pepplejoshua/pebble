package backend

import (
	"bytes"
	"regexp"
	"testing"
)

// Phase 3 #19 — tagged-union variant literal construction in a bare switch
// case body. These tests prove, end to end through the compile-link-run
// harness, that a union variant is constructible as the RETURN VALUE of a
// bare (brace-less) single-statement switch case body — `case .value: return
// C.value(c.value + 1);` — across every payload shape the payload gate admits
// and for a generic union. The gap: buildSwitchCaseBody's bare-return path
// used to re-dispatch only the char/str/aggregate/slice/float return shapes
// and fell through to buildExpr for everything else, so a VariantConstruct
// (or plain enum value, bool, array, ...) returned from a bare case body hit
// buildExpr's integer-width gate ("want int") even though the block-body form
// (`case .value: { return C.value(5); }`) and the fall-through path both
// already routed returns through the complete buildReturnStatement. The fix
// delegates the bare-return path to buildReturnStatement.
//
// Every helper below switches on a union-typed parameter, constructs a fresh
// variant in each bare case body, and returns it; the entry reads the result
// back through a narrowed read. An exit code of 42 means the constructed
// value round-tripped through construction in the bare case body -> return ->
// narrowed read. Each payload's constructed value is chosen to distinguish
// the construction from the input (e.g. the .value arm maps payload v to
// v+1), so a case body that merely forwarded its input would fail.

// TestEmitTaggedUnionVariantLiteralBareCaseReturnCompileAndRun proves the
// bare-case-body construction fix for every scalar payload shape
// unionPayloadCTypeAdmissible admits (Phase 3 #19): int, str, bool, char,
// f64, and a plain enum payload. Each union's helper constructs a fresh
// variant in each bare case body and returns it; the narrowed read-back
// recovers the exact constructed value. The zero-payload sibling shape (a
// payload-less `C.empty()` construction returned from a bare case body) is
// the last subtest.
func TestEmitTaggedUnionVariantLiteralBareCaseReturnCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"int-payload", "type C = union enum { empty void; value int; }; fn mk(c C) C { switch c { case .empty: return C.value(0); case .value: return C.value(c.value + 1); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { return rd(mk(C.value(41))); }"},
		{"str-payload", `type C = union enum { empty void; value str; }; fn mk(c C) C { switch c { case .empty: return C.value("no"); case .value: return C.value("hi"); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == "hi" { return 42; } return 0; } } } fn main() int { return rd(mk(C.value("x"))); }`},
		{"bool-payload", "type C = union enum { empty void; value bool; }; fn mk(c C) C { switch c { case .empty: return C.value(false); case .value: return C.value(true); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value { return 42; } return 0; } } } fn main() int { return rd(mk(C.value(true))); }"},
		{"char-payload", "type C = union enum { empty void; value char; }; fn mk(c C) C { switch c { case .empty: return C.value('z'); case .value: return C.value('a'); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == 'a' { return 42; } return 0; } } } fn main() int { return rd(mk(C.value('a'))); }"},
		{"float-payload", "type C = union enum { empty void; value f64; }; fn mk(c C) C { switch c { case .empty: return C.value(0.0); case .value: return C.value(2.5); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == 2.5 { return 42; } return 0; } } } fn main() int { return rd(mk(C.value(1.5))); }"},
		{"enum-payload", "type Color = enum { red, green, blue }; type C = union enum { empty void; value Color; }; fn mk(c C) C { switch c { case .empty: return C.value(Color.red); case .value: return C.value(Color.green); } } fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == Color.green { return 42; } return 0; } } } fn main() int { return rd(mk(C.value(Color.blue))); }"},
		{"zero-payload", "type C = union enum { empty void; value int; }; fn mk(c C) C { switch c { case .empty: return C.empty(); case .value: return C.empty(); } } fn rd(c C) int { switch c { case .empty: return 42; case .value: return -1; } } fn main() int { var x C = C.empty(); var y C = mk(x); return rd(y); }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitTaggedUnionNestedUnionVariantLiteralBareCaseReturnCompileAndRun
// proves a nested tagged union is constructible as a bare-case-body return:
// each arm constructs an Inner variant inline as the payload of an Outer
// construction (`Outer.value(Inner.b(42))`) and returns it. Inner is reached
// in the construct-then-reference form the current union typedef emission
// order requires (the inline-only form is a separate, already-tracked
// typedef-ordering gap), so the entry binds the input Inner to a local
// before the outer construction. The narrowed read-back recovers the inner
// payload 42.
func TestEmitTaggedUnionNestedUnionVariantLiteralBareCaseReturnCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Inner = union enum { a void; b int; };
type Outer = union enum { empty void; value Inner; };
fn mk(o Outer) Outer { switch o { case .empty: return Outer.value(Inner.a()); case .value: return Outer.value(Inner.b(42)); } }
fn rd2(i Inner) int { switch i { case .a: return -1; case .b: return i.b; } }
fn rd(o Outer) int { switch o { case .empty: return -1; case .value: return rd2(o.value); } }
fn main() int { let i = Inner.b(0); var o Outer = Outer.value(i); let m = mk(o); return rd(m); }`, false, 42, false)
}

// TestEmitTaggedUnionGenericVariantLiteralBareCaseReturnCompileAndRun proves
// a GENERIC union is constructible as a bare-case-body return — the exact
// std/result.peb map shape: each arm constructs a fresh Result[U, E] with a
// generic-typed payload (`Result[U, str].{ Ok = f(r.Ok) }`) in a bare case
// body. This is the shape that proved the gap was live in real std-library
// code, not just a synthetic fixture. map2(r, inc) maps Ok 41 to 42; the
// entry reads m.Ok through a narrowed switch.
func TestEmitTaggedUnionGenericVariantLiteralBareCaseReturnCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Result[T, E] = union enum { Ok T; Err E; };
fn inc(x int) int { return x + 1; }
fn map2[U](r Result[int, str], f fn(int) U) Result[U, str] {
    switch r {
        case .Ok: return Result[U, str].{ Ok = f(r.Ok) };
        case .Err: return Result[U, str].{ Err = r.Err };
    }
}
fn main() int {
    let r = Result[int, str].{ Ok = 41 };
    let m = map2(r, inc);
    switch m {
        case .Ok: return m.Ok;
        case .Err: return -1;
    }
}`, false, 42, false)
}

// TestEmitPlainEnumVariantLiteralBareCaseReturnCompileAndRun locks in the
// direct sibling of the tagged-union fix: a plain ENUM variant literal is
// also a bare-case-body return (`case .green: return Color.blue;`), routed by
// the same buildReturnStatement delegation buildSwitchCaseBody now uses.
// pick maps green -> blue; the entry checks the mapping.
func TestEmitPlainEnumVariantLiteralBareCaseReturnCompileAndRun(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; fn pick(c Color) Color { switch c { case .red: return Color.green; case .green: return Color.blue; case .blue: return Color.red; } } fn main() int { if pick(Color.green) == Color.blue { return 42; } return 0; }", false, 42, false)
}

// TestEmitTaggedUnionVariantLiteralBareCaseReturnBlockControl proves the
// block-body form — `case .value: { return C.value(c.value + 1); }` — which
// worked before the fix (it routes through buildBlock -> buildReturnStatement)
// still works after the delegation, so the two case-body spellings agree.
func TestEmitTaggedUnionVariantLiteralBareCaseReturnBlockControl(t *testing.T) {
	emitAndRun(t, "type C = union enum { empty void; value int; }; fn mk(c C) C { switch c { case .empty: { return C.value(0); } case .value: { return C.value(c.value + 1); } } } fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { return rd(mk(C.value(41))); }", false, 42, false)
}

// TestEmitGenericTaggedUnionTwoSpecializationsCompilesAndRun proves that two
// live specializations of one generic tagged union can each be constructed
// independently without enumerator name collisions: Result[int, str] and
// Result[bool, str] are both instantiated, an Ok[int] and an Ok[bool] are
// built, and the program exits 0. Before the fix both specializations emitted
// the same bare pebble_variant_N constants, causing duplicate-symbol errors
// or misrouted switches.
func TestEmitGenericTaggedUnionTwoSpecializationsCompilesAndRun(t *testing.T) {
	emitAndRun(t, `type Result[T, E] = union enum { Ok T; Err E; };
fn main() int {
    var a Result[int, str] = Result[int, str].{ Ok = 5 };
    var b Result[bool, str] = Result[bool, str].{ Ok = true };
    return 0;
}`, false, 0, false)
}

// TestEmitGenericTaggedUnionTwoSpecializationsDistinctEnumVariantNames proves
// that the two specializations of a generic tagged union emit DISTINCT
// pebble_variant_<typeID>_<memberID> constant names in their tag-enums, not
// the same bare pebble_variant_N repeated. Each specialization gets its own
// type ID, so the generated C must contain two separate typedefs with
// non-overlapping variant names (e.g. pebble_variant_XX_YY vs
// pebble_variant_ZZ_WW where XX != ZZ).
func TestEmitGenericTaggedUnionTwoSpecializationsDistinctEnumVariantNames(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, `type Result[T, E] = union enum { Ok T; Err E; };
fn main() int {
    var a Result[int, str] = Result[int, str].{ Ok = 5 };
    var b Result[bool, str] = Result[bool, str].{ Ok = true };
    return 0;
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Collect all pebble_variant_XXX_YYY identifiers from the output.
	// Each unique <typeID>_<memberID> pair should appear exactly once as a
	// constant definition (the typedef), and then be referenced in cases /
	// compound literals. The key invariant: there must be at least two
	// DIFFERENT type prefixes among the variant names.
	matches := regexp.MustCompile(`pebble_variant_(\d+_\d+)`).FindAllStringSubmatch(out, -1)
	if len(matches) == 0 {
		t.Fatalf("emitted C contains no pebble_variant_<typeID>_<memberID> identifiers:\n%s", out)
	}
	prefixes := make(map[string]bool)
	for _, m := range matches {
		prefixes[m[1]] = true
	}
	if len(prefixes) < 2 {
		t.Errorf("emitted C has only %d distinct pebble_variant prefixes, want >= 2 (two specializations must have distinct names):\n%s", len(prefixes), out)
	}
}
