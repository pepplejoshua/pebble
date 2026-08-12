package backend

import "testing"

// Phase 3 #12 — tagged-union variant selection (switch subject, and a union
// value read directly used as a call argument) off any value source, not just
// a simple local. buildUnionValueExpr (values.go) previously handled only a
// union-typed struct field read (Load/FieldPlace) — a whole union read
// through a pointer deref, a union field of a non-addressable struct value, a
// force-unwrap, and a parenthesized union value all now resolve through it
// too, and buildSwitchStatement's union-subject branch (statements.go) routes
// every such shape through the same builder, appending the parenthesized
// `.tag` projection. Enum-typed variant selection off these same shapes
// already worked (proven here as regression guards, not new fixes).

// TestUnionSwitchSubjectOffNonLocalSource proves a tagged-union switch
// subject built from every value source beyond a simple local: a struct
// field read, a force-unwrap, a parenthesized local, a field of a
// non-addressable call result, a pointer deref, and a two-level nested
// aggregate field read off a call result.
func TestUnionSwitchSubjectOffNonLocalSource(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"field-read", "type C = union enum { empty void; value i32; }; type S = struct { u C; }; fn main() i32 { var s S = S.{ u = C.value(42) }; switch s.u { case .empty: return -1; case .value: return s.u.value; } }"},
		{"force-unwrap", "type C = union enum { empty void; value i32; }; fn main() i32 { var o ?C = some C.value(42); switch o! { case .empty: return -1; case .value: return o!.value; } }"},
		{"parens", "type C = union enum { empty void; value i32; }; fn main() i32 { let c C = C.value(42); switch (c) { case .empty: return -1; case .value: return c.value; } }"},
		{"field-value-off-call-result", "type C = union enum { empty void; value i32; }; type S = struct { u C; }; fn mk() S { return S.{ u = C.value(42) }; } fn main() i32 { switch mk().u { case .empty: return -1; case .value: return 42; } }"},
		{"deref", "type C = union enum { empty void; value i32; }; fn main() i32 { var c C = C.value(42); var p *C = &c; switch *p { case .empty: return -1; case .value: return 42; } }"},
		{"nested-aggregate-off-call-result", "type C = union enum { empty void; value i32; }; type Inner = struct { u C; }; type Outer = struct { inner Inner; }; fn mk() Outer { return Outer.{ inner = Inner.{ u = C.value(42) } }; } fn main() i32 { switch mk().inner.u { case .empty: return -1; case .value: return 42; } }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestUnionValueOffNonLocalSourceAsCallArgument proves a union value read
// from a non-local source — a non-addressable struct field and a pointer
// deref — used directly as a call argument (not just a switch subject),
// exercising buildUnionValueExpr's FieldValue and Load(DereferencePlace)
// cases from a second call site.
func TestUnionValueOffNonLocalSourceAsCallArgument(t *testing.T) {
	emitAndRun(t, "type C = union enum { empty void; value i32; }; type S = struct { u C; }; fn mk() S { return S.{ u = C.value(42) }; } fn rd(u C) i32 { switch u { case .empty: return -1; case .value: return u.value; } } fn main() i32 { return rd(mk().u); }", false, 42, false)
	emitAndRun(t, "type C = union enum { empty void; value i32; }; fn rd(u C) i32 { switch u { case .empty: return -1; case .value: return u.value; } } fn main() i32 { var c C = C.value(42); var p *C = &c; return rd(*p); }", false, 42, false)
}

// TestEnumSwitchSubjectOffNonLocalSource proves the enum-typed counterparts
// of the union shapes above (a slice element, a nested aggregate off a call
// result, a pointer deref, and a parenthesized local) as regression guards —
// these already worked before Phase 3 #12, unaffected by the union-side fix.
func TestEnumSwitchSubjectOffNonLocalSource(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"slice-element", "type E = enum { A, B }; fn main() i32 { let es []E = [.A, .B]; switch es[1] { case .A: return -1; case .B: return 42; } }"},
		{"nested-aggregate-off-call-result", "type E = enum { A, B }; type Inner = struct { e E; }; type Outer = struct { inner Inner; }; fn mk() Outer { return Outer.{ inner = Inner.{ e = .B } }; } fn main() i32 { switch mk().inner.e { case .A: return -1; case .B: return 42; } }"},
		{"deref", "type E = enum { A, B }; fn main() i32 { var e E = .B; var p *E = &e; switch *p { case .A: return -1; case .B: return 42; } }"},
		{"parens", "type E = enum { A, B }; fn main() i32 { let e E = .B; switch (e) { case .A: return -1; case .B: return 42; } }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}
