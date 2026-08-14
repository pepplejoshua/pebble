package backend

import "testing"

// Phase 3 #8 — tagged-union variant payload widening (the scalar payload
// shapes). These tests prove, end to end through the compile-link-run harness,
// that every payload shape unionPayloadCTypeAdmissible admits is genuinely
// wireable: the variant is constructed (Choice.value(<payload>)) and the
// payload is read back through a switch case's narrowed variant read
// (`c.value` inside `case .value:`), the only channel the language offers for
// observing a stored payload. An exit code of 42 means the narrowed read
// recovered the exact constructed value; any other code is a deliberate
// wrong-value or wrong-branch check that fires when the payload did not round
// trip.

// TestEmitTaggedUnionIntegerPayloadCompileAndRun proves every fixed-width
// integer builtin is a wireable tagged-union payload (Phase 3 #8): i8, i16,
// i32, i64, u8, u16, u32, u64, int, and uint. Each is constructed as a union
// variant's payload, read back through the narrowed `c.value` read, and
// compared against the exact constructed value. The values are chosen to
// exceed the next-narrower width's range (300 for i16/u16, 1000 for i64) so a
// payload that was mis-resolved to a narrower C type would fail the comparison;
// the entry exits 42 only on a faithful round trip. (The backend emits each
// integer payload at its own C width — int8_t through uint64_t — via
// unionMemberCType, independent of the ambient entry width.)
func TestEmitTaggedUnionIntegerPayloadCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"i8", "type C = union enum { empty void; value i8; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(7); if rd(c) == 7 { return 42; } return 0; }"},
		{"i16", "type C = union enum { empty void; value i16; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(300); if rd(c) == 300 { return 42; } return 0; }"},
		{"i32", "type C = union enum { empty void; value i32; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(42); if rd(c) == 42 { return 42; } return 0; }"},
		{"i64", "type C = union enum { empty void; value i64; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(1000); if rd(c) == 1000 { return 42; } return 0; }"},
		{"u8", "type C = union enum { empty void; value u8; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(9); if rd(c) == 9 { return 42; } return 0; }"},
		{"u16", "type C = union enum { empty void; value u16; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(600); if rd(c) == 600 { return 42; } return 0; }"},
		{"u32", "type C = union enum { empty void; value u32; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(42); if rd(c) == 42 { return 42; } return 0; }"},
		{"u64", "type C = union enum { empty void; value u64; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(42); if rd(c) == 42 { return 42; } return 0; }"},
		{"int", "type C = union enum { empty void; value int; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value; return x; } } } fn main() int { let c = C.value(42); if rd(c) == 42 { return 42; } return 0; }"},
		{"uint", "type C = union enum { empty void; value uint; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { var x int = c.value as int; return x; } } } fn main() int { let c = C.value(42); if rd(c) == 42 { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitTaggedUnionBoolPayloadCompileAndRun proves bool is a wireable
// tagged-union payload: the payload member is declared bool (unionMemberCType),
// the construction builds the payload under the bool grammar
// (buildBoolExpr), and the narrowed read lowers to the union typedef's bool
// payload projection. Both truth values round trip — a true payload fires the
// if, a false payload falls through to the 42 arm — proving the stored value,
// not just the tag, is what the narrowed read observes.
func TestEmitTaggedUnionBoolPayloadCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"true", "type C = union enum { empty void; value bool; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value { return 42; } return 0; } } } fn main() int { let c = C.value(true); return rd(c); }"},
		{"false", "type C = union enum { empty void; value bool; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value { return 0; } return 42; } } } fn main() int { let c = C.value(false); return rd(c); }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitTaggedUnionCharPayloadCompileAndRun proves char is a wireable
// tagged-union payload: the payload member is declared int32_t (the char C
// type everywhere, see unionMemberCType), the construction builds the payload
// under the char grammar (buildCharOperand), and the narrowed read is a Load
// whose place is a FieldPlace — the exact shape buildCharOperand's new
// FieldPlace case resolves to the union typedef's char payload projection.
// Comparing the read-back payload against the constructed literal 'a' proves
// the char value round trips through construction and the narrowed switch.
func TestEmitTaggedUnionCharPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, "type C = union enum { empty void; value char; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == 'a' { return 42; } return 0; } } } fn main() int { let c = C.value('a'); return rd(c); }", false, 42, false)
}

// TestEmitTaggedUnionStrPayloadCompileAndRun proves str is a wireable
// tagged-union payload: the payload member is declared PebbleStr (the runtime
// ABI's fixed str C type), the construction builds the payload under the str
// grammar (buildStrOperand), and the narrowed read compares the recovered
// PebbleStr against the constructed literal.
func TestEmitTaggedUnionStrPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, "type C = union enum { empty void; value str; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == \"hi\" { return 42; } return 0; } } } fn main() int { let c = C.value(\"hi\"); return rd(c); }", false, 42, false)
}

// TestEmitTaggedUnionFloatPayloadCompileAndRun proves both float builtins are
// wireable tagged-union payloads: f32's payload member is declared float and
// f64's double (unionMemberCType), each construction builds its payload under
// the float grammar at its own kind (buildFloatExpr), and the narrowed read
// recovers the exact constructed value. The f64 value 2.5 is not exactly
// representable in f32, so a payload mis-resolved to the wrong float width
// would fail the comparison.
func TestEmitTaggedUnionFloatPayloadCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"f32", "type C = union enum { empty void; value f32; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == 1.5 { return 42; } return 0; } } } fn main() int { let c = C.value(1.5); return rd(c); }"},
		{"f64", "type C = union enum { empty void; value f64; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == 2.5 { return 42; } return 0; } } } fn main() int { let c = C.value(2.5); return rd(c); }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitTaggedUnionEnumPayloadCompileAndRun proves a plain enum is a wireable
// tagged-union payload: the payload member is declared with the enum's own
// pebble_enum_<typeID>_t typedef (unionMemberCType, emitted in the enum block
// that leads the union block), the construction builds the payload under the
// enum grammar (buildEnumValue), and the narrowed read compares the recovered
// enum value against a variant constant. The enum-typed narrowed payload read
// is compared directly in the case body — the enum Load(FieldPlace) shape
// buildEnumValue supports; binding it into an enum-typed local or passing it as
// an enum-typed call argument are separate, unrelated tracker gaps this test
// deliberately avoids.
func TestEmitTaggedUnionEnumPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, "type Color = enum { red, green, blue }; type C = union enum { empty void; value Color; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: { if c.value == Color.green { return 42; } return 0; } } } fn main() int { let c = C.value(Color.green); return rd(c); }", false, 42, false)
}

// TestEmitTaggedUnionNestedUnionPayloadCompileAndRun proves a nested tagged
// union is a wireable tagged-union payload: the outer union's payload member is
// declared with the inner union's own pebble_union_<typeID>_t typedef, the
// construction builds the payload under the union grammar (buildUnionValueExpr),
// and the narrowed read passes the recovered inner union value to a helper that
// switches on it. The inner union is constructed into a local before the outer
// construction so the collection walk records Inner before Outer — the current
// union typedef block is emitted in first-encountered order, so the inner
// union's typedef must precede the outer's that references it; an inline
// `Outer.value(Inner.b(7))` construction that reaches Inner only through the
// outer's payload child is a separate typedef-ordering tracker gap this test
// deliberately avoids.
func TestEmitTaggedUnionNestedUnionPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Inner = union enum { a void; b int; };
type Outer = union enum { empty void; value Inner; };
fn is_b(i Inner) int { switch i { case .a: return 1; case .b: return 42; } }
fn rd(c Outer) int {
    switch c {
        case .empty: return -1;
        case .value: return is_b(c.value);
    }
}
fn main() int { let i = Inner.b(7); let c = Outer.value(i); return rd(c); }`, false, 42, false)
}

// TestEmitTaggedUnionNestedUnionPayloadInlineCompileAndRun proves an INLINE
// nested tagged-union payload construction works end to end: the inner union is
// constructed directly inside the outer's payload argument
// (`Outer.value(Inner.b(7))`) with no intermediate local, in the three
// positions the scalar payload tests cover — a local declaration, a function
// argument, and a return value. The collection walk reaches the inner union
// only through the outer's payload child (there is no separate
// Inner-constructing statement to record it first), so the union typedef block
// must emit the inner union's typedef before the outer's that inline-references
// it — the dependency-first emission this test proves. A three-level nesting
// (`Outer.value(Mid.value(Deep.b(7)))`) is included as the same DFS postorder
// emission falls out of the fix for free.
func TestEmitTaggedUnionNestedUnionPayloadInlineCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"local-declaration", `type Inner = union enum { a void; b int; };
type Outer = union enum { empty void; value Inner; };
fn is_b(i Inner) int { switch i { case .a: return 1; case .b: return 42; } }
fn rd(c Outer) int {
    switch c {
        case .empty: return -1;
        case .value: return is_b(c.value);
    }
}
fn main() int { let c = Outer.value(Inner.b(7)); return rd(c); }`},
		{"function-argument", `type Inner = union enum { a void; b int; };
type Outer = union enum { empty void; value Inner; };
fn is_b(i Inner) int { switch i { case .a: return 1; case .b: return 42; } }
fn rd(c Outer) int {
    switch c {
        case .empty: return -1;
        case .value: return is_b(c.value);
    }
}
fn main() int { return rd(Outer.value(Inner.b(7))); }`},
		{"return-value", `type Inner = union enum { a void; b int; };
type Outer = union enum { empty void; value Inner; };
fn is_b(i Inner) int { switch i { case .a: return 1; case .b: return 42; } }
fn rd(c Outer) int {
    switch c {
        case .empty: return -1;
        case .value: return is_b(c.value);
    }
}
fn mk() Outer { return Outer.value(Inner.b(7)); }
fn main() int { return rd(mk()); }`},
		{"three-level-nesting", `type Deep = union enum { a void; b int; };
type Mid = union enum { empty void; value Deep; };
type Outer = union enum { empty void; value Mid; };
fn is_b(i Deep) int { switch i { case .a: return 1; case .b: return 42; } }
fn rm(m Mid) int { switch m { case .empty: return -1; case .value: return is_b(m.value); } }
fn rd(c Outer) int {
    switch c {
        case .empty: return -1;
        case .value: return rm(c.value);
    }
}
fn main() int { let c = Outer.value(Mid.value(Deep.b(7))); return rd(c); }`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitTaggedUnionPayloadPositionsCompileAndRun proves the scalar payload
// shapes are wireable in every construction position the Phase 3 test files
// cover: a local declaration (`var c C = C.value(42)`), a function argument
// (`rd(C.value(42))`), and a return value (`fn mk() C { return C.value(42);
// }`), each followed by a narrowed read-back through a helper switch. The
// 42 = 42 + 0 composition proves each position's payload independently.
func TestEmitTaggedUnionPayloadPositionsCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"local-declaration", "type C = union enum { empty void; value int; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { var c C = C.value(42); return rd(c); }"},
		{"function-argument", "type C = union enum { empty void; value int; }; fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { return rd(C.value(42)); }"},
		{"return-value", "type C = union enum { empty void; value int; }; fn mk() C { return C.value(42); } fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { return rd(mk()); }"},
		{"argument-and-return", "type C = union enum { empty void; value int; }; fn mk() C { return C.value(42); } fn rd(c C) int { switch c { case .empty: return -1; case .value: return c.value; } } fn main() int { return rd(mk()) + rd(C.value(0)); }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitRejectsAggregateUnionPayloadAtBackend confirms the Phase 3 #8
// checker fix (internal/check/call_facts.go, prepareVariant): a literal
// aggregate payload — an array literal (`Choice.value([1, 2, 3])`) or an
// optional literal (`Choice.value(some 5)`) — now passes the CHECKER (its
// elements are anchored to the declared payload type instead of a C0601), and
// the rejection happens only in the BACKEND, whose deliberate scope limit
// (unionPayloadCTypeAdmissible) admits exactly the scalar shapes the tests
// above prove and cleanly rejects every aggregate payload shape. buildFixture
// would fail the test if the checker rejected the program, so reaching
// assertEmitRejectsContaining with the backend's own message — "which is not
// supported as a tagged-union payload" — is itself the proof the checker fix
// landed: the array/optional payloads type-check but are not yet wireable at
// the current typedef order (union typedefs lead the aggregate block, so an
// aggregate payload would need the reverse order).
func TestEmitRejectsAggregateUnionPayloadAtBackend(t *testing.T) {
	t.Parallel()
	emitAndRunRejects(t, "type C = union enum { empty void; value [3]i32; }; fn main() i32 { var c C = C.value([1, 2, 3]); return 0; }", "carries a payload of type [3]i32, which is not supported as a tagged-union payload")
	emitAndRunRejects(t, "type C = union enum { empty void; value ?i32; }; fn main() i32 { var c C = C.value(some 5); return 0; }", "carries a payload of type ?i32, which is not supported as a tagged-union payload")
}

// F5-17 — an ordinary (non-nested) struct as a tagged-union variant payload
// (`Shape.rect(Point.{ x = 3, y = 4 })`). The union typedef's payload member
// names the struct's own pebble_struct_<typeID>_t typedef, so Emit hoists the
// self-contained plain-struct typedef ahead of the union block that references
// it (see Emit's union-payload-struct hoisting); the construction builds the
// struct literal payload through the struct-value grammar and the narrowed
// read recovers it into a struct local through the union-payload FieldPlace
// projection. An exit code of 42 (or the repro's own 7) means the payload
// round-tripped through construction and the narrowed read.

// TestEmitTaggedUnionStructPayloadCompileAndRun is the exact F5-17 repro: a
// union enum with a void variant and one struct-typed variant, constructed
// inline (`Shape.rect(Point.{ x = 3, y = 4 })`), discriminated by a switch,
// and read back through the narrowed variant read into a struct local
// (`var p Point = s.rect; return p.x + p.y;`). The exit code 7 is 3 + 4 — the
// sum of the two recovered fields — so the payload survived construction AND
// the narrowed read with both fields intact.
func TestEmitTaggedUnionStructPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Point = struct { x int; y int; };
type Shape = union enum { empty void; rect Point; };
fn main() int {
    let s = Shape.rect(Point.{ x = 3, y = 4 });
    switch s {
    case .empty: return -1;
    case .rect: { var p Point = s.rect; return p.x + p.y; }
    }
}`, false, 7, false)
}

// TestEmitTaggedUnionMultiFieldStructPayloadCompileAndRun proves ALL fields of
// a plain struct payload survive construction and the narrowed read, not just
// one: the struct carries four fields of four different scalar types (int,
// bool, i64, str), each constructed to a distinguishing value and each
// verified by the narrowed read-back (40 + 1 for b + 1 for c + 0 for d = 42).
// A payload that lost any one field — a wrong-width int, a dropped bool, a
// mis-resolved wide integer, or a lost str — would fail the sum.
func TestEmitTaggedUnionMultiFieldStructPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Rec = struct { a int; b bool; c i64; d str; };
type C = union enum { empty void; value Rec; };
fn rd(c C) int {
    switch c {
        case .empty: return -1;
        case .value: {
            var r Rec = c.value;
            var t int = r.a;
            if r.b { t = t + 1; }
            if r.c == 900 { t = t + 1; }
            if r.d == "ok" { t = t + 0; }
            return t;
        }
    }
}
fn main() int { let c = C.value(Rec.{ a = 40, b = true, c = 900, d = "ok" }); return rd(c); }`, false, 42, false)
}

// TestEmitTaggedUnionMultiVariantStructPayloadCompileAndRun proves a union
// with multiple variants — one carrying a struct payload, one a scalar
// payload, and one no payload at all — discriminates correctly: the switch on
// the void variant returns -1, the switch on the int variant returns its
// exact payload, and the switch on the struct variant returns the sum of the
// struct fields. Each arm is constructed and read back in turn, so the
// struct-typed member coexists with the scalar/void members without affecting
// their C layout or dispatch.
func TestEmitTaggedUnionMultiVariantStructPayloadCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Point = struct { x int; y int; };
type Shape = union enum { empty void; num int; rect Point; };
fn rd(s Shape) int {
    switch s {
        case .empty: return -1;
        case .num: return s.num;
        case .rect: { var p Point = s.rect; return p.x + p.y; }
    }
}
fn main() int {
    var a Shape = Shape.empty();
    if rd(a) != -1 { return 1; }
    var b Shape = Shape.num(41);
    if rd(b) != 41 { return 2; }
    var c Shape = Shape.rect(Point.{ x = 20, y = 22 });
    return rd(c);
}`, false, 42, false)
}

// TestEmitRejectsNestedStructUnionPayloadAtBackend confirms the F5-17 slice
// boundary: a struct payload that itself carries a NESTED aggregate field (a
// struct-in-struct) is deliberately OUT OF SCOPE and cleanly rejected at
// collection time — it is not plain (see isPlainStructPayload), so its
// typedef cannot be hoisted ahead of the union block, and the backend rejects
// it rather than emitting a mis-ordered typedef. The rejection names the
// payload type in the same message every unsupported payload shape uses.
func TestEmitRejectsNestedStructUnionPayloadAtBackend(t *testing.T) {
	t.Parallel()
	emitAndRunRejects(t, "type Inner = struct { a int; }; type Outer = struct { i Inner; x int; }; type C = union enum { empty void; value Outer; }; fn main() i32 { let c = C.value(Outer.{ i = Inner.{ a = 1 }, x = 2 }); return 0; }", "carries a payload of type nominal(")
}
