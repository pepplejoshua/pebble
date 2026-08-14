package backend

import "testing"

// Untagged-union (`union { ... }`, NominalUnion) end-to-end coverage. Every
// test drives real .peb source through the full check pipeline, Emit, the cc
// compile-link step, and an actual run against the runtime, asserting either
// the program's exit code (42 means the asserted value round-tripped / the
// asserted bit pattern was observed) or — for the out-of-scope non-scalar
// boundary — that Emit rejects cleanly.
//
// The semantics under test are deliberately unsafe: an untagged union's fields
// share one C union storage with no tag and no discriminant, so reading a
// field the construction did not set returns the raw bytes written through
// whichever field WAS set. That is what the reinterpretation tests below prove
// (i32 -1 read back through a u32 field as 4294967295, and siblings), and what
// the write-then-read tests prove for reassignment (writing one field makes
// its bytes visible through a DIFFERENT field's read).
//
// char union fields were previously excluded from the round-trip table below
// because READING a char field back failed at Emit ("field N has type char,
// want a fixed-width integer, bool, pointer, or enum, or str") —
// buildStructFieldRead / buildStructFieldValueRead had no char case — but that
// gap is now closed, so char entries are included alongside the other scalars.

// TestEmitUntaggedUnionScalarRoundTripCompileAndRun proves each supported
// scalar field kind round-trips through construction-and-read-back: construct
// the union with exactly one field, read that SAME field, and confirm the
// value survived. Exit code 42 means the round trip preserved the value. The
// unsigned entries use their full-width maxima (255/65535/4294967295/
// 18446744073709551615) and the signed entries a negative value, so a value
// that never made it into the storage, or that was truncated to a default
// width, would fail the comparison.
func TestEmitUntaggedUnionScalarRoundTripCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"int", "type D = union { a int; }; fn main() i32 { var d D = D.{ a = -7 }; if d.a == -7 { return 42; } return 0; }"},
		{"i8", "type D = union { a i8; }; fn main() i32 { var d D = D.{ a = -8 }; if d.a == -8 { return 42; } return 0; }"},
		{"i16", "type D = union { a i16; }; fn main() i32 { var d D = D.{ a = -8 }; if d.a == -8 { return 42; } return 0; }"},
		{"i32", "type D = union { a i32; }; fn main() i32 { var d D = D.{ a = -8 }; if d.a == -8 { return 42; } return 0; }"},
		{"i64", "type D = union { a i64; }; fn main() i32 { var d D = D.{ a = -8 }; if d.a == -8 { return 42; } return 0; }"},
		{"uint", "type D = union { a uint; }; fn main() i32 { var d D = D.{ a = 18446744073709551615 }; if d.a == 18446744073709551615 { return 42; } return 0; }"},
		{"u8", "type D = union { a u8; }; fn main() i32 { var d D = D.{ a = 255 }; if d.a == 255 { return 42; } return 0; }"},
		{"u16", "type D = union { a u16; }; fn main() i32 { var d D = D.{ a = 65535 }; if d.a == 65535 { return 42; } return 0; }"},
		{"u32", "type D = union { a u32; }; fn main() i32 { var d D = D.{ a = 4294967295 }; if d.a == 4294967295 { return 42; } return 0; }"},
		{"u64", "type D = union { a u64; }; fn main() i32 { var d D = D.{ a = 18446744073709551615 }; if d.a == 18446744073709551615 { return 42; } return 0; }"},
		{"bool", "type D = union { a bool; }; fn main() i32 { var d D = D.{ a = true }; if d.a { return 42; } return 0; }"},
		{"char", "type D = union { a char; }; fn main() i32 { var d D = D.{ a = 'x' }; if d.a == 'x' { return 42; } return 0; }"},
		{"char-non-ascii", "type D = union { a char; }; fn main() i32 { var d D = D.{ a = 'é' }; if d.a == 'é' { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitUntaggedUnionAnonymousConstructionCompileAndRun proves the
// anonymous construction form — `var d D = .{ a = 42 };` with no base-type
// name — reaches the same aggregateUnion construction path end to end. The
// checker's memberByName re-derives the field's member against the solved
// destination declaration (see member_validation_test.go's untagged-union
// construction tests), so the anonymous form must compile and run exactly as
// the qualified Data.{ a = 42 } form does.
func TestEmitUntaggedUnionAnonymousConstructionCompileAndRun(t *testing.T) {
	emitAndRun(t, `type D = union { a i32; b u32; };
fn main() i32 {
    var d D = .{ a = 42 };
    return d.a;
}`, false, 42, false)
}

// TestEmitUntaggedUnionReinterpretationCompileAndRun proves the deliberate
// unsafe semantics are real: construct the union through ONE field, then read
// a DIFFERENT field of the same byte width and confirm the RAW BIT PATTERN is
// returned, not a safe zero/error. Three width pairings pin the behavior as
// general rather than an accident of one specific pair: the canonical
// i32(-1) -> u32(4294967295) case plus i8(-1) -> u8(255) and i64(-1) ->
// u64(18446744073709551615). A "safe" implementation (zero-initializing the
// unset field, or rejecting the cross-field read) would return 0 and fail
// every comparison.
func TestEmitUntaggedUnionReinterpretationCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"i32-to-u32", "type D = union { a i32; b u32; }; fn main() i32 { var d D = D.{ a = -1 }; if d.b == 4294967295 { return 42; } return 0; }"},
		{"i8-to-u8", "type D = union { a i8; b u8; }; fn main() i32 { var d D = D.{ a = -1 }; if d.b == 255 { return 42; } return 0; }"},
		{"i64-to-u64", "type D = union { a i64; b u64; }; fn main() i32 { var d D = D.{ a = -1 }; if d.b == 18446744073709551615 { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitUntaggedUnionWriteThenReadDifferentFieldCompileAndRun proves the
// bytes written through one field are visible through a DIFFERENT field's
// read after an actual REASSIGNMENT, not just at construction time. Each case
// constructs with field a, then writes a field (either a or b) and reads the
// OTHER, confirming the write's bits are what the read returns. The second
// case covers the reverse write direction (write u32, read i32) so the
// byte-for-byte view is exercised in both signed/unsigned directions.
func TestEmitUntaggedUnionWriteThenReadDifferentFieldCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"write-i32-read-u32", "type D = union { a i32; b u32; }; fn main() i32 { var d D = D.{ a = 5 }; d.a = -1; if d.b == 4294967295 { return 42; } return 0; }"},
		{"write-u32-read-i32", "type D = union { a i32; b u32; }; fn main() i32 { var d D = D.{ a = 5 }; d.b = 4294967295; if d.a == -1 { return 42; } return 0; }"},
		{"write-i64-read-u64", "type D = union { a i64; b u64; }; fn main() i32 { var d D = D.{ a = 5 }; var v i64 = -1; d.a = v; if d.b == 18446744073709551615 { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitUntaggedUnionParameterAndReturnCompileAndRun proves a union is a
// genuine value in the function grammar: passable to a helper by value,
// returnable by value, and mutable through a parameter with the mutation
// visible to the caller via the returned union (the direct C union
// pass-by-value contract). mk maps a scalar to a construction, rd reads a
// field back through the parameter, and bump reassigns a field on its
// parameter before returning it.
func TestEmitUntaggedUnionParameterAndReturnCompileAndRun(t *testing.T) {
	emitAndRun(t, `type D = union { a i32; b u32; };
fn mk(v i32) D { return D.{ a = v }; }
fn rd(d D) i32 { return d.a; }
fn bump(d D) D { d.a = d.a + 1; return d; }
fn main() i32 {
    var d D = mk(41);
    var e D = bump(d);
    return rd(e);
}`, false, 42, false)
}

// TestEmitUntaggedUnionNestedStructFieldCompileAndRun proves an untagged
// union is constructible as the value of a struct field (`Holder.{ u =
// Data.{ a = 42 } }`), that the nested field round-trips through the struct's
// own construction machinery, and that the unsafe reinterpretation semantics
// survive the nesting (constructing the nested union with a = -1 makes the
// struct's u.b read return 4294967295). The anonymous nested form
// (`Holder.{ u = .{ a = 42 } }`) is cleanly rejected at CHECK time with an
// unresolved-inference-variable diagnostic (the field destination carries no
// declared-type grounding for an anonymous union literal), so the qualified
// form is the supported spelling.
func TestEmitUntaggedUnionNestedStructFieldCompileAndRun(t *testing.T) {
	emitAndRun(t, `type D = union { a i32; b u32; };
type Holder = struct { u D; };
fn main() i32 {
    var h Holder = Holder.{ u = D.{ a = 42 } };
    return h.u.a;
}`, false, 42, false)
	emitAndRun(t, `type D = union { a i32; b u32; };
type Holder = struct { u D; };
fn main() i32 {
    var h Holder = Holder.{ u = D.{ a = -1 } };
    if h.u.b == 4294967295 { return 42; }
    return 0;
}`, false, 42, false)
}

// TestEmitUntaggedUnionGenericInstantiateI32CompileAndRun is the exact
// regression repro for F5-02: a generic untagged union instantiated with a
// concrete scalar payload (i32) must compile and run, returning the stored
// value. Before the fix, the member's type reached the union typedef builder
// as the raw type-parameter symbol instead of being substituted to i32.
func TestEmitUntaggedUnionGenericInstantiateI32CompileAndRun(t *testing.T) {
	emitAndRun(t, `type Box[T] = union {
    a T;
    b int;
};
fn main() i32 {
    var d Box[i32] = Box[i32].{ a = 5 };
    return d.a;
}`, false, 5, false)
}

// TestEmitUntaggedUnionGenericInstantiateBoolCompileAndRun proves the fix
// is not width-specific: a generic untagged union instantiated with bool
// also compiles and runs correctly.
func TestEmitUntaggedUnionGenericInstantiateBoolCompileAndRun(t *testing.T) {
	emitAndRun(t, `type Box[T] = union {
    a T;
    b int;
};
fn main() i32 {
    var d Box[bool] = Box[bool].{ a = true };
    if d.a {
        return 42;
    }
    return 0;
}`, false, 42, false)
}

// TestEmitUntaggedUnionGenericInstantiateU64CompileAndRun proves the fix
// works for u64 as well. Uses in-program comparison because 999 exceeds the
// valid exit-code range (0-255) — 999 mod 256 = 231 — so returning it
// directly as the exit code would never match regardless of correctness.
func TestEmitUntaggedUnionGenericInstantiateU64CompileAndRun(t *testing.T) {
	emitAndRun(t, `type Box[T] = union {
    a T;
    b int;
};
fn main() i32 {
    var d Box[u64] = Box[u64].{ a = 999 };
    if d.a == 999 {
        return 42;
    }
    return 0;
}`, false, 42, false)
}

// TestEmitUntaggedUnionGenericTwoInstantiationsInOneProgram proves two
// different instantiations of the same generic untagged union both live in
// one program, each gets its own correctly-substituted typedef, and both
// produce correct results. This also exercises whether the same
// duplicate-naming class of bug found for tagged unions during F5-01
// affects untagged unions.
func TestEmitUntaggedUnionGenericTwoInstantiationsInOneProgram(t *testing.T) {
	emitAndRun(t, `type Box[T] = union {
    a T;
    b int;
};
fn main() i32 {
    var d Box[i32] = Box[i32].{ a = 5 };
    var e Box[bool] = Box[bool].{ a = true };
    if d.a == 5 {
        if e.a {
            return 42;
        }
    }
    return 0;
}`, false, 42, false)
}

// TestEmitUntaggedUnionGenericInstantiateNonScalarRejects pins the
// out-of-scope boundary for generic untagged unions: when instantiated with
// a non-scalar concrete type (a struct), the union typedef builder must
// cleanly reject with the "not supported" message naming the CORRECT
// substituted type (the concrete struct's typedef name), not a raw
// type-parameter symbol. This confirms the substitution IS happening — it's
// just that the resulting concrete type fails the scalar-only gate.
func TestEmitUntaggedUnionGenericInstantiateNonScalarRejects(t *testing.T) {
	emitAndRunRejects(t, `type Inner = struct { x i32; };
type Box[T] = union {
    a T;
    b int;
};
fn main() i32 { var d Box[Inner] = Box[Inner].{ a = Inner.{ x = 1 } }; return 0; }`, "not supported")
}

// TestEmitUntaggedUnionRejectsNonScalarFieldCompileAndRun pins the explicit
// out-of-scope boundary of this slice: a struct field and an array field
// inside an untagged union are cleanly rejected at Emit with the
// "not supported" message (this slice supports only scalar fixed-width
// integer, uint, bool, and char union fields) — never a crash and never
// invalid C emission. Both fixtures construct the non-scalar field, so the
// rejection comes from the union construction/typedef path.
func TestEmitUntaggedUnionRejectsNonScalarFieldCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"struct-field", `type Inner = struct { x i32; };
type D = union { a Inner; b i32; };
fn main() i32 { var d D = D.{ a = Inner.{ x = 1 } }; return 0; }`},
		{"array-field", `type D = union { a [3]i32; b i32; };
fn main() i32 { var d D = D.{ a = [1, 2, 3] }; return 0; }`},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRunRejects(t, tc.source, "not supported")
		})
	}
}
