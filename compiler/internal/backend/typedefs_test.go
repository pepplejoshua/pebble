package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitGenericStructDataFieldsWritesConcreteCTypedefs(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check: each specialization's typedef field C types
	// must match its concrete instantiation — int32_t for the int-typed fields
	// of Pair[int, int] AND of Pair[int, bool]'s key, bool for Pair[int,
	// bool]'s value — with no generic placeholder and no rejection. The two
	// typedefs are distinct pebble_struct_<typeID>_t definitions (25 for
	// Pair[int, int], 26 for Pair[int, bool], 27/28 the key/value field
	// symbols) from a real fixture dump. The entry's resolved width here is
	// types.Int, which cType maps to int32_t.
	unit, snapshot, entryID, sources := buildFixture(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; let q Pair[int, bool] = Pair[int, bool].{ key = 6, value = true }; if q.value { return p.key + p.value; } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_27;\n    int32_t pebble_field_28;\n} pebble_struct_25_t;",
		"typedef struct {\n    int32_t pebble_field_27;\n    bool pebble_field_28;\n} pebble_struct_26_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// The two specializations are distinct typedef names: the second does not
	// reuse the first's (a single shared layout would emit only one typedef).
	if strings.Count(out, "} pebble_struct_25_t;") != 1 || strings.Count(out, "} pebble_struct_26_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two specializations:\n%s", out)
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("struct typedefs do not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitGenericStructPointerTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check for the pointer two-specialization case: each
	// specialization's typedef must declare the CORRECT pointee C type — int32_t
	// for Ref[int], bool for Ref[bool] — with no shared/wrong pointee and no
	// rejection. The two typedefs are distinct pebble_struct_<typeID>_t
	// definitions (24 for Ref[int], 25 for Ref[bool], 26 the ptr field symbol)
	// from a real fixture dump. Before the fix both typedefs declared the same
	// pointee (one specialization's won and the other was silently wrong).
	unit, snapshot, entryID, sources := buildFixture(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var s Ref[bool] = Ref[bool].{ ptr = nil }; var x int = 7; var y bool = true; var p *int = &x; var q *bool = &y; r.ptr = p; s.ptr = q; if s.ptr == nil { return 0; } else { if *s.ptr { return *r.ptr; } else { return 1; } } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t * pebble_field_26;\n} pebble_struct_24_t;",
		"typedef struct {\n    bool * pebble_field_26;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Each specialization is a distinct typedef name and the second does not
	// reuse the first's pointee (a shared layout would emit one typedef).
	if strings.Count(out, "} pebble_struct_24_t;") != 1 || strings.Count(out, "} pebble_struct_25_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two specializations:\n%s", out)
	}
}

func TestEmitGenericStructOptionalTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check for the optional two-specialization case: each
	// specialization's typedef must name its OWN payload optional type —
	// pebble_optional_30_t (Optional(int)) for Box[int], pebble_optional_31_t
	// (Optional(bool)) for Box[bool] — and BOTH optional typedefs must be
	// emitted (before this slice the bool-payload optional was referenced but
	// never defined, a real cc error). Struct type IDs 24/25, optional types
	// 30/31, field symbol 26 from a real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; var c Box[bool] = Box[bool].{ value = some true }; var d Box[bool] = Box[bool].{ value = none }; if c.value! { if d.value! { return 1; } else { return b.value!; } } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    pebble_optional_30_t pebble_field_26;\n} pebble_struct_24_t;",
		"typedef struct {\n    pebble_optional_31_t pebble_field_26;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Both optional typedefs must exist: the bool-payload optional is what was
	// referenced-but-undefined before this slice.
	if strings.Count(out, "} pebble_optional_30_t;") != 1 || strings.Count(out, "} pebble_optional_31_t;") != 1 {
		t.Errorf("expected exactly one typedef each for the two optional payloads:\n%s", out)
	}
}

func TestEmitGenericStructNestedFieldWritesInnerTypedefFirst(t *testing.T) {
	t.Parallel()
	// The emitted-C shape and ORDER check for the nested-generic case: the
	// inner struct's typedef must be emitted BEFORE the outer struct's, since
	// C requires a type to be fully defined before it is used as a by-value
	// member (a forward declaration is not enough for an embedded field).
	// orderAggregateTypes's DFS postorder emits dependencies first, so once
	// the fix collects Inner[int] at all, `pebble_struct_26_t` (Inner[int])
	// precedes `pebble_struct_25_t` (Outer[int], whose field names
	// pebble_struct_26_t). The order is asserted directly rather than trusting
	// the compile (which would also fail loudly under -Werror were it wrong).
	// Struct type IDs 26/25 and field symbols 26 (val) / 29 (inner) from a
	// real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; return o.inner.val; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_26;\n} pebble_struct_26_t;",
		"typedef struct {\n    pebble_struct_26_t pebble_field_29;\n} pebble_struct_25_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Recover the inner typedef name from the outer struct's struct-typed
	// field reference, then assert the inner typedef's definition precedes
	// that reference (dependency-first emission).
	innerName := ""
	for _, line := range strings.Split(out, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "pebble_struct_") && strings.Contains(trimmed, " pebble_field_") {
			innerName = strings.TrimSpace(strings.Fields(trimmed)[0])
			break
		}
	}
	if innerName == "" {
		t.Fatalf("emitted C has no struct-typed field reference:\n%s", out)
	}
	innerTypedefEnd := strings.Index(out, "} "+innerName+";")
	outerFieldRef := strings.Index(out, innerName+" pebble_field_")
	if innerTypedefEnd < 0 {
		t.Errorf("emitted C missing the inner struct typedef definition (%s):\n%s", innerName, out)
	} else if outerFieldRef < 0 {
		t.Errorf("emitted C missing the outer struct's struct-typed field reference:\n%s", out)
	} else if innerTypedefEnd > outerFieldRef {
		t.Errorf("inner struct typedef (%s) is not emitted before the outer struct that embeds it (inner typedef end %d > outer field reference %d):\n%s", innerName, innerTypedefEnd, outerFieldRef, out)
	}
}

func TestEmitGenericStructNestedFieldTwoSpecializationsWriteConcreteCTypedefs(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check for the nested-generic two-specialization case:
	// each outer specialization's inner field must name ITS OWN nested
	// specialization's typedef — pebble_struct_26_t (Inner[int]) inside
	// pebble_struct_25_t (Outer[int]), pebble_struct_28_t (Inner[bool])
	// inside pebble_struct_27_t (Outer[bool]) — with no shared/wrong inner
	// typedef (a shared layout would emit one). Struct type IDs 26/25/28/27,
	// field symbols 26 (val) / 29 (inner) from a real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; var b Outer[bool] = Outer[bool].{ inner = Inner[bool].{ val = true } }; if b.inner.val { return o.inner.val; } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_26;\n} pebble_struct_26_t;",
		"typedef struct {\n    pebble_struct_26_t pebble_field_29;\n} pebble_struct_25_t;",
		"typedef struct {\n    bool pebble_field_26;\n} pebble_struct_28_t;",
		"typedef struct {\n    pebble_struct_28_t pebble_field_29;\n} pebble_struct_27_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Four distinct typedef names — the two specializations do not share
	// layouts (each outer embeds its own inner).
	for _, name := range []string{"pebble_struct_26_t", "pebble_struct_25_t", "pebble_struct_28_t", "pebble_struct_27_t"} {
		if strings.Count(out, "} "+name+";") != 1 {
			t.Errorf("expected exactly one typedef named %s:\n%s", name, out)
		}
	}
}

func TestEmitTupleParameterParamOnlyTypeGetsTypedef(t *testing.T) {
	t.Parallel()
	// The typedef-discovery extension: the (i32, i32) tuple type appears ONLY
	// as sumT's parameter type — sumT is never called (so no reachable body
	// constructs a tuple of that type) and main constructs no tuple at all —
	// yet the typedef must still be discovered, because the emitted helper's C
	// signature names pebble_tuple_<typeID>_t. Before 10.24's Parameters scan
	// in collectTupleTypes this returned nothing; the test drives
	// collectTupleTypes directly with a hand-built reachable-helper slice, so
	// it fails if the discovery stops being tied to a construction site. (The
	// concrete type ID 23 is confirmed from the fixture dump.)
	unit, snapshot, entryID, _ := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	sumTDecl, err := findFunctionDeclaration(unit, 24, "called function")
	if err != nil {
		t.Fatalf("sumT declaration: %v", err)
	}
	_, sumTBody, err := findFunctionBody(unit, sumTDecl, "called function")
	if err != nil {
		t.Fatalf("sumT body: %v", err)
	}
	helpers := []helperInfo{{decl: sumTDecl, block: sumTBody}}
	ids, err := collectTupleTypes(unit, snapshot, entryBlock, helpers)
	if err != nil {
		t.Fatalf("collectTupleTypes failed: %v", err)
	}
	found := false
	for _, id := range ids {
		if id == 23 {
			found = true
		}
	}
	if !found {
		t.Fatalf("tuple type 23 used only as a parameter type was not discovered, got %v", ids)
	}
}

func TestEmitOptionalUintTypedefWritesUint64T(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check for the uint payload: the optional typedef
	// declares the .value field as uint64_t (the C type uint resolves to),
	// never int32_t or a rejection, and the some construction assigns the
	// "u"-suffixed literal into it.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var o ?uint = some 5; return o! as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "typedef struct {\n    bool has_value;\n    uint64_t value;\n} pebble_optional_") {
		t.Errorf("emitted C does not declare the uint payload's .value field as uint64_t:\n%s", out)
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared an i32 .value field for a uint payload:\n%s", out)
	}
	if !strings.Contains(out, ".has_value = true, .value = 5u") {
		t.Errorf("emitted C is missing the uint-payload some construction (.value = 5u):\n%s", out)
	}
	if !strings.Contains(out, "pebble_rt_checked_unwrap_u64(") {
		t.Errorf("emitted C is missing the u64-width force-unwrap helper call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitOptionalPointerTypedefWritesPointeePointerCType(t *testing.T) {
	t.Parallel()
	// The emitted-C shape check for the pointer payload: the optional typedef
	// declares the .value field as the pointee's pointer C type (int32_t * for
	// ?*int, via pointerTypeName), never a rejection or a scalar, and the some
	// construction assigns the AddressOf expression into it; the force-unwrap
	// routes to the new pebble_rt_checked_unwrap_ptr helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var y int = 7; var o ?*int = some &y; if !o.has_value { return 99; } return *(o!); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "typedef struct {\n    bool has_value;\n    int32_t * value;\n} pebble_optional_") {
		t.Errorf("emitted C does not declare the pointer payload's .value field as int32_t *:\n%s", out)
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared a scalar .value field for a pointer payload:\n%s", out)
	}
	if !strings.Contains(out, ".has_value = true, .value = (int32_t *)(&pebble_local_") {
		t.Errorf("emitted C is missing the pointer-payload some construction (.value = AddressOf):\n%s", out)
	}
	if !strings.Contains(out, "pebble_rt_checked_unwrap_ptr(") {
		t.Errorf("emitted C is missing the pointer-width force-unwrap helper call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitNestedTypedefOrderWritesAndCompiles(t *testing.T) {
	t.Parallel()
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let t (Point, i32) = (p, 1); return t.0.x + t.0.y; }"
	unit, snapshot, entryID, sources := buildFixture(t, src, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatal(err)
	}
	out := buf.String()
	inner := strings.Index(out, "pebble_struct_")
	outer := strings.Index(out, "pebble_tuple_")
	if inner < 0 || outer < 0 || inner > outer {
		t.Fatalf("nested typedef dependency order is wrong:\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_") || !strings.Contains(out, "._0") {
		t.Fatalf("nested access chain missing:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitStructParameterParamOnlyTypeGetsTypedef(t *testing.T) {
	t.Parallel()
	// The typedef-discovery extension, struct side: the Point type appears ONLY
	// as f's parameter type — f is never called (so no reachable body
	// constructs a Point of that type) and main constructs no struct at all —
	// yet the typedef must still be discovered, because the emitted helper's C
	// signature names pebble_struct_<typeID>_t. Before 10.24's Parameters scan
	// in collectStructTypes this returned nothing; the test drives
	// collectStructTypes directly with a hand-built reachable-helper slice, so
	// it fails if the discovery stops being tied to a construction site. (The
	// concrete type ID 23 is confirmed from the fixture dump; the callee reads
	// both fields, so resolveStructInfo has every field's type available.)
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	fDecl, err := findFunctionDeclaration(unit, 27, "called function")
	if err != nil {
		t.Fatalf("f declaration: %v", err)
	}
	_, fBody, err := findFunctionBody(unit, fDecl, "called function")
	if err != nil {
		t.Fatalf("f body: %v", err)
	}
	helpers := []helperInfo{{decl: fDecl, block: fBody}}
	infos, err := collectStructTypes(&emitState{}, unit, snapshot, entryBlock, helpers, nil)
	if err != nil {
		t.Fatalf("collectStructTypes failed: %v", err)
	}
	found := false
	for _, info := range infos {
		if info.typ == 23 {
			found = true
		}
	}
	if !found {
		t.Fatalf("struct type 23 used only as a parameter type was not discovered, got %+v", infos)
	}
}

func TestEmitTupleResultTypeScanGetsTypedef(t *testing.T) {
	t.Parallel()
	// The ResultType scan in collectTupleTypes, proven load-bearing: the
	// (i32, i32) tuple type is used ONLY as makeT's result type, and the
	// helpers slice pairs makeT's declaration with main's body block — a real,
	// valid Block that contains no tuple construction (main is `return 0;`), so
	// the body walk finds nothing and makeT's Parameters are empty. The only
	// path by which collectTupleTypes can discover type 23 is the helper's own
	// ResultType; without 10.26's ResultType scan this returns nothing and the
	// test fails. (The concrete type ID 23 is confirmed from the fixture dump.)
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { return 0; }", "main", false)
	entryDecl, err := findFunctionDeclaration(unit, entryID, "entry function")
	if err != nil {
		t.Fatalf("entry declaration: %v", err)
	}
	_, entryBlock, err := findFunctionBody(unit, entryDecl, "entry function")
	if err != nil {
		t.Fatalf("entry body: %v", err)
	}
	makeTDecl, err := findFunctionDeclaration(unit, 24, "called function")
	if err != nil {
		t.Fatalf("makeT declaration: %v", err)
	}
	// Pair makeT's declaration with main's tuple-free body block, isolating the
	// ResultType scan as the sole discovery path for tuple type 23.
	helpers := []helperInfo{{decl: makeTDecl, block: entryBlock}}
	ids, err := collectTupleTypes(unit, snapshot, entryBlock, helpers)
	if err != nil {
		t.Fatalf("collectTupleTypes failed: %v", err)
	}
	found := false
	for _, id := range ids {
		if id == 23 {
			found = true
		}
	}
	if !found {
		t.Fatalf("tuple type 23 used only as a helper's result type was not discovered, got %v", ids)
	}
}
