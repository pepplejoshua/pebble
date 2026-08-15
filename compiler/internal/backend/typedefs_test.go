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
	// types.Int, which cType maps to int64_t.
	unit, snapshot, entryID, sources := buildFixture(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; let q Pair[int, bool] = Pair[int, bool].{ key = 6, value = true }; if q.value { return p.key + p.value; } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int64_t pebble_field_27;\n    int64_t pebble_field_28;\n} pebble_struct_25_t;",
		"typedef struct {\n    int64_t pebble_field_27;\n    bool pebble_field_28;\n} pebble_struct_26_t;",
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
	// specialization's typedef must declare the CORRECT pointee C type — int64_t
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
		"typedef struct {\n    int64_t * pebble_field_26;\n} pebble_struct_24_t;",
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
	// pebble_optional_25_t (Optional(int)) for Box[int], pebble_optional_27_t
	// (Optional(bool)) for Box[bool] — and BOTH optional typedefs must be
	// emitted (before this slice the bool-payload optional was referenced but
	// never defined, a real cc error). Struct type IDs 24/26, optional types
	// 25/27, field symbol 26 from a real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; var c Box[bool] = Box[bool].{ value = some true }; var d Box[bool] = Box[bool].{ value = none }; if c.value! { if d.value! { return 1; } else { return b.value!; } } else { return 0; } }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    pebble_optional_25_t pebble_field_26;\n} pebble_struct_24_t;",
		"typedef struct {\n    pebble_optional_27_t pebble_field_26;\n} pebble_struct_26_t;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	// Both optional typedefs must exist: the bool-payload optional is what was
	// referenced-but-undefined before this slice.
	if strings.Count(out, "} pebble_optional_25_t;") != 1 || strings.Count(out, "} pebble_optional_27_t;") != 1 {
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
		"typedef struct {\n    int64_t pebble_field_26;\n} pebble_struct_26_t;",
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
		"typedef struct {\n    int64_t pebble_field_26;\n} pebble_struct_26_t;",
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
	// declares the .value field as the pointee's pointer C type (int64_t * for
	// ?*int, via pointerTypeName), never a rejection or a scalar, and the some
	// construction assigns the AddressOf expression into it; the force-unwrap
	// routes to the new pebble_rt_checked_unwrap_ptr helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var y int = 7; var o ?*int = some &y; if !o.has_value { return 99; } return *(o!); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "typedef struct {\n    bool has_value;\n    int64_t * value;\n} pebble_optional_") {
		t.Errorf("emitted C does not declare the pointer payload's .value field as int64_t *:\n%s", out)
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared a scalar .value field for a pointer payload:\n%s", out)
	}
	if !strings.Contains(out, ".has_value = true, .value = (int64_t *)(&pebble_local_") {
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
		if info.typ == 19 {
			found = true
		}
	}
	if !found {
		t.Fatalf("struct type 19 used only as a parameter type was not discovered, got %+v", infos)
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

// structTypedefNames returns, in emission order, every full struct typedef's
// name (`} pebble_struct_<typeID>_t;`) in the emitted C. Only the closing line
// of a COMPLETE struct typedef matches the prefix; union/tuple/optional/array
// typedefs and slice forward declarations use different names or shapes, so the
// list is exactly the aggregate block's struct typedefs in dependency-first
// postorder.
func structTypedefNames(out string) []string {
	var names []string
	for _, line := range strings.Split(out, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "} pebble_struct_") {
			names = append(names, strings.TrimSuffix(strings.TrimPrefix(trimmed, "} "), ";"))
		}
	}
	return names
}

func TestEmitThreeLevelStructChainCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The reproduction this whole change exists for: a plain struct-only
	// dependency chain Outer -> Middle -> Inner (three levels, no arrays) must
	// now compile and run, returning 42. Before the selective depth fix,
	// orderAggregateTypes rejected it as "more than one level of nesting".
	unit, snapshot, entryID, sources := buildFixture(t, "type Inner = struct { value int; };\ntype Middle = struct { inner Inner; };\ntype Outer = struct { middle Middle; };\nfn main() int { let o = Outer.{ middle = Middle.{ inner = Inner.{ value = 42 } } }; return o.middle.inner.value; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitThreeLevelStructChainTypedefsDependencyFirst(t *testing.T) {
	t.Parallel()
	// The structural ordering check for the three-level chain: the emitted C
	// must define the inner struct's typedef before the middle's, and the
	// middle's before the outer's, because each struct embeds the previous one
	// by value (C requires a type fully defined before it is used as a struct
	// member). The first three struct typedefs are exactly Inner, Middle,
	// Outer in postorder; the test recovers their names from the emitted C
	// rather than hardcoding TypeIDs, then asserts both the order and that each
	// outer typedef's struct-typed field names the immediately preceding one.
	unit, snapshot, entryID, sources := buildFixture(t, "type Inner = struct { value int; };\ntype Middle = struct { inner Inner; };\ntype Outer = struct { middle Middle; };\nfn main() int { let o = Outer.{ middle = Middle.{ inner = Inner.{ value = 42 } } }; return o.middle.inner.value; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	names := structTypedefNames(out)
	if len(names) < 3 {
		t.Fatalf("expected at least three struct typedefs, got %v:\n%s", names, out)
	}
	inner, middle, outer := names[0], names[1], names[2]
	innerEnd := strings.Index(out, "} "+inner+";")
	middleEnd := strings.Index(out, "} "+middle+";")
	outerEnd := strings.Index(out, "} "+outer+";")
	if innerEnd < 0 || middleEnd < 0 || outerEnd < 0 {
		t.Fatalf("failed to locate the three struct typedef definitions:\n%s", out)
	}
	if !(innerEnd < middleEnd && middleEnd < outerEnd) {
		t.Fatalf("struct typedefs are not dependency-first: Inner end %d, Middle end %d, Outer end %d:\n%s", innerEnd, middleEnd, outerEnd, out)
	}
	// Middle's typedef embeds Inner by value and Outer's embeds Middle.
	middleField := strings.Index(out, inner+" pebble_field_")
	outerField := strings.Index(out, middle+" pebble_field_")
	if middleField < 0 || middleField > middleEnd {
		t.Fatalf("middle struct typedef does not embed the inner typedef before its own end (field ref %d, middle end %d):\n%s", middleField, middleEnd, out)
	}
	if outerField < 0 || outerField > outerEnd {
		t.Fatalf("outer struct typedef does not embed the middle typedef before its own end (field ref %d, outer end %d):\n%s", outerField, outerEnd, out)
	}
}

func TestEmitFourLevelStructChainCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A four-level struct-only chain (Top -> Outer -> Middle -> Inner) is a
	// strictly deeper case than the reproduction, confirming the selective fix
	// is not a three-level special case: any depth built purely from struct
	// nesting is allowed and emits in dependency-first order.
	unit, snapshot, entryID, sources := buildFixture(t, "type Inner = struct { value int; };\ntype Middle = struct { inner Inner; };\ntype Outer = struct { middle Middle; };\ntype Top = struct { outer Outer; };\nfn main() int { let t = Top.{ outer = Outer.{ middle = Middle.{ inner = Inner.{ value = 42 } } } }; return t.outer.middle.inner.value; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitTupleAndOptionalNestedInStructChainCompiles(t *testing.T) {
	t.Parallel()
	// The selective fix must also cover a struct-only chain that routes through
	// tuple and optional wrappers (depth 2, no arrays): Wrapper -> (Pair, int)
	// tuple and Wrapper -> ?Pair optional, both down to Pair. All four typedefs
	// (Pair struct, tuple, optional, Wrapper struct) must be emitted
	// dependency-first and the program must compile and run.
	unit, snapshot, entryID, sources := buildFixture(t, "type Pair = struct { a int; b int; };\ntype Wrapper = struct { t (Pair, int); o ?Pair; };\nfn main() int { let w = Wrapper.{ t = (Pair.{ a = 20, b = 22 }, 1), o = some Pair.{ a = 5, b = 6 } }; return w.t.0.a + w.t.0.b; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Pair's struct typedef must be defined before the tuple and optional
	// typedefs that embed it, and both of those before the Wrapper struct
	// typedef that embeds them.
	pairName := ""
	for _, line := range strings.Split(out, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "} pebble_struct_") {
			pairName = strings.TrimSuffix(strings.TrimPrefix(trimmed, "} "), ";")
			break
		}
	}
	if pairName == "" {
		t.Fatalf("emitted C has no struct typedef:\n%s", out)
	}
	pairEnd := strings.Index(out, "} "+pairName+";")
	tupleRef := strings.Index(out, pairName+" _0;")
	optionalRef := strings.Index(out, pairName+" value;")
	if pairEnd < 0 || tupleRef < 0 || optionalRef < 0 {
		t.Fatalf("failed to locate Pair typedef or its tuple/optional embeddings:\n%s", out)
	}
	if !(pairEnd < tupleRef && pairEnd < optionalRef) {
		t.Fatalf("Pair typedef (%s, end %d) is not emitted before the tuple/optional typedefs that embed it (tuple ref %d, optional ref %d):\n%s", pairName, pairEnd, tupleRef, optionalRef, out)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitArrayOfAggregateStructFieldStillRejected(t *testing.T) {
	t.Parallel()
	// The selective fix must NOT newly enable the array-of-aggregate shape:
	// `struct { arr [2]Inner; }` routes its dependency chain through an array,
	// so the depth>1 rejection must still fire. Allowing it would emit the
	// field-referenced array typedef BEFORE the aggregate block (see Emit's
	// fieldArrayTypedefs) while its inline `pebble_struct_<Inner> data[2]`
	// member names a struct typedef not yet defined — a silent C-ordering bug.
	// The element struct must be a genuinely NON-plain aggregate for the guard
	// to fire: F5-18 admits a PLAIN struct element (its typedef names only
	// scalar C types and is emitted dependency-first in the aggregate block),
	// so this fixture nests Inner into another struct (Inner -> Outer), which
	// keeps the depth>1-through-array rejection. The shape must be CONSTRUCTED
	// (reachable) for the check to run; an unreferenced Holder never reaches
	// orderAggregateTypes.
	unit, snapshot, entryID, sources := buildFixture(t, "type Outer = struct { value int; };\ntype Inner = struct { outer Outer; };\ntype Holder = struct { arr [2]Inner; };\nfn main() int { let h = Holder.{ arr = [Inner.{ outer = Outer.{ value = 1 } }, Inner.{ outer = Outer.{ value = 2 } }] }; return h.arr[0].outer.value; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err == nil {
		t.Fatal("Emit accepted a struct whose field is an array of an aggregate, which would emit the array typedef before the element struct typedef is defined")
	} else if !strings.Contains(err.Error(), "more than one level of nesting") {
		t.Fatalf("unexpected rejection: %v", err)
	}
}

func TestEmitArrayOfPlainStructFieldCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// The target of this change: Path contains [3]Point where Point is a
	// plain struct (only scalar fields). Before the fix this was rejected as
	// "more than one level of nesting"; after the fix it should compile and
	// run, returning 7 (p.points[0].x + p.points[2].y = 1 + 6 — points[2] is
	// { x = 5, y = 6 }, so its y field is 6).
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\ntype Path = struct { points [3]Point; };\nfn main() i32 { var p Path = Path.{ points = [Point.{ x = 1, y = 2 }, Point.{ x = 3, y = 4 }, Point.{ x = 5, y = 6 }] }; return p.points[0].x + p.points[2].y; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Verify all three typedefs exist: Point, Path, and the array type.
	if !strings.Contains(out, "} pebble_struct_") || strings.Count(out, "} pebble_struct_") < 2 {
		t.Fatalf("expected at least two struct typedefs:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitArrayOfPlainStructMultiFieldCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A struct with 3+ fields proves no field is lost across the whole array.
	// Point3D has three distinct fields, and the program reads every field at
	// every index. All fields are i32 (the source language rejects mixing i32
	// and int in one addition), and the expected sum 45 fits the 8-bit process
	// exit code the harness asserts on (an earlier 666 truncated to 154).
	unit, snapshot, entryID, sources := buildFixture(t, "type Point3D = struct { x i32; y i32; z i32; };\ntype Line = struct { pts [3]Point3D; };\nfn main() i32 { var l Line = Line.{ pts = [Point3D.{ x = 1, y = 2, z = 3 }, Point3D.{ x = 4, y = 5, z = 6 }, Point3D.{ x = 7, y = 8, z = 9 }] }; var sum i32 = 0; sum += l.pts[0].x + l.pts[0].y + l.pts[0].z; sum += l.pts[1].x + l.pts[1].y + l.pts[1].z; sum += l.pts[2].x + l.pts[2].y + l.pts[2].z; return sum; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	// Expected: (1+2+3) + (4+5+6) + (7+8+9) = 6 + 15 + 24 = 45
	compileAndRun(t, buf.Bytes(), 45, false)
}

func TestEmitArrayOfNestedStructStillRejected(t *testing.T) {
	t.Parallel()
	// Genuinely unsupported case: Holder contains [2]Inner where Inner itself
	// nests further into another struct (Inner -> Child). This must STILL
	// cleanly reject with a clear error, not a crash or silent miscompile.
	unit, snapshot, entryID, sources := buildFixture(t, "type Child = struct { val i32; };\ntype Inner = struct { child Child; };\ntype Holder = struct { arr [2]Inner; };\nfn main() i32 { var h Holder = Holder.{ arr = [Inner.{ child = Child.{ val = 1 } }, Inner.{ child = Child.{ val = 2 } }] }; return h.arr[0].child.val; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err == nil {
		t.Fatal("Emit accepted a struct whose field is an array of a struct that itself nests further — this should be rejected")
	} else if !strings.Contains(err.Error(), "more than one level of nesting") {
		t.Fatalf("unexpected rejection message: %v", err)
	}
}

// F5-19 — a plain struct as a first-class function type parameter
// (`var f fn(Point) int = sum_point`). The function typedef names the struct's
// own pebble_struct_<typeID>_t typedef in its parameter list, so Emit hoists
// the self-contained plain-struct typedef ahead of the function block that
// references it (see Emit's typedef-ordering hoisting); the indirect call
// builds its argument through the same buildCallArgument path used for ordinary
// helper calls (which already has a working isStruct case via buildAggregateArgument).
// An exit code of 7 (or the repro's own 7) means the struct round-tripped
// through the function-type parameter with all fields intact.

// TestEmitFunctionTypePlainStructParamCompileAndRun is the exact F5-19 repro:
// a plain Point struct passed as a parameter to a first-class function value.
// The function is called indirectly through a function-typed local, passing a
// constructed Point literal. The exit code 7 is 3 + 4 — the sum of the two
// recovered fields — so the struct parameter survived the indirect call.
func TestEmitFunctionTypePlainStructParamCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x int; y int; };
fn sum_point(p Point) int {
    return p.x + p.y;
}
fn main() int {
    var f fn(Point) int = sum_point;
    var p Point = Point.{ x = 3, y = 4 };
    return f(p);
}`, false, 7, false)
}

// TestEmitFunctionTypePlainStructMultiFieldCompileAndRun proves ALL fields of
// a plain struct parameter survive an indirect call, not just one: the struct
// carries four fields of four different scalar types (int, bool, i64, str),
// each passed through a function-typed local and each verified by the callee.
// The sum 42 = 40 + 1 (b) + 1 (c) + 0 (d) confirms no field is lost.
func TestEmitFunctionTypePlainStructMultiFieldCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Rec = struct { a int; b bool; c i64; d str; };
fn rd(f fn(Rec) int, r Rec) int {
    return f(r);
}
fn calc(r Rec) int {
    var t int = r.a;
    if r.b { t = t + 1; }
    if r.c == 900 { t = t + 1; }
    if r.d == "ok" { t = t + 0; }
    return t;
}
fn main() int {
    var f fn(Rec) int = calc;
    var r Rec = Rec.{ a = 40, b = true, c = 900, d = "ok" };
    return rd(f, r);
}`, false, 42, false)
}

// TestEmitRejectsNonPlainStructFunctionParamAtBackend confirms the F5-19 slice
// boundary: a struct parameter that itself carries a NESTED aggregate field
// (a struct-in-struct) is deliberately OUT OF SCOPE and cleanly rejected at
// validation time — it is not plain (see isPlainStructField), so its typedef
// cannot be hoisted ahead of the function block, and the backend rejects it
// rather than emitting a mis-ordered typedef. The rejection names the
// parameter position and type.
func TestEmitRejectsNonPlainStructFunctionParamAtBackend(t *testing.T) {
	t.Parallel()
	emitAndRunRejects(t, "type Inner = struct { val int; };\ntype Outer = struct { inner Inner; x int; };\nfn get_x(p Outer) int { return p.x; }\nfn main() int { var f fn(Outer) int = get_x; return 0; }", "function type fn(nominal(symbol ")
}

// TestEmitFunctionTypeOptionalParamStillRejected confirms that optional
// parameters in function types remain cleanly rejected.
func TestEmitFunctionTypeOptionalParamStillRejected(t *testing.T) {
	t.Parallel()
	emitAndRunRejects(t, "fn get_val(x ?int) int { return 0; }\nfn main() int { var f fn(?int) int = get_val; return 0; }", "function type")
}

// TestEmitFunctionTypePlainStructResultCompileAndRun is the exact F5-20 repro:
// a plain Point struct returned from a first-class function value. The function
// is called indirectly through a function-typed local, and the struct result is
// bound to a struct-typed local. The exit code 7 is 3 + 4 — the sum of the two
// recovered fields — so the struct result survived the indirect call.
func TestEmitFunctionTypePlainStructResultCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x int; y int; };

fn make_point() Point {
    return Point.{ x = 3, y = 4 };
}

fn main() int {
    var f fn() Point = make_point;
    var p Point = f();
    return p.x + p.y;
}`, false, 7, false)
}

// TestEmitFunctionTypePlainStructResultMultiFieldCompileAndRun proves ALL fields
// of a plain struct result survive an indirect call, not just one: the struct
// carries four fields of four different scalar types (int, bool, i64, str), each
// returned through a function-typed local and each verified by the caller. The
// sum 42 = 40 + 1 (b) + 1 (c) + 0 (d) confirms no field is lost.
func TestEmitFunctionTypePlainStructResultMultiFieldCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Rec = struct { a int; b bool; c i64; d str; };

fn mk_rec() Rec {
    return Rec.{ a = 40, b = true, c = 900, d = "ok" };
}

fn main() int {
    var f fn() Rec = mk_rec;
    var r Rec = f();
    var t int = r.a;
    if r.b { t = t + 1; }
    if r.c == 900 { t = t + 1; }
    if r.d == "ok" { t = t + 0; }
    return t;
}`, false, 42, false)
}

// TestEmitRejectsNonPlainStructFunctionResultAtBackend confirms the F5-20 slice
// boundary: a struct result that itself carries a NESTED aggregate field
// (a struct-in-struct) is deliberately OUT OF SCOPE and cleanly rejected at
// validation time — it is not plain (see isPlainStructField), so its typedef
// cannot be hoisted ahead of the function block, and the backend rejects it
// rather than emitting a mis-ordered typedef. The rejection names the
// result position and type.
func TestEmitRejectsNonPlainStructFunctionResultAtBackend(t *testing.T) {
	t.Parallel()
	emitAndRunRejects(t, "type Inner = struct { val int; };\ntype Outer = struct { inner Inner; x int; };\nfn get_outer() Outer { return Outer.{ inner = Inner.{ val = 1 }, x = 2 }; }\nfn main() int { var f fn() Outer = get_outer; return 0; }", "has result type nominal(symbol ")
}

// TestEmitFunctionTypePlainStructResultChainedCompileAndRun proves a plain struct
// result can flow through multiple levels of indirect calls: main calls f which
// returns Point, then passes that Point through g (which takes Point as param and
// returns Point) via another function-typed local, verifying the struct survives
// both the result and parameter positions across chained indirect calls.
func TestEmitFunctionTypePlainStructResultChainedCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `type Point = struct { x int; y int; };

fn make_point() Point {
    return Point.{ x = 3, y = 4 };
}

fn add_point(p Point) Point {
    return Point.{ x = p.x + 1, y = p.y + 2 };
}

fn main() int {
    var f fn() Point = make_point;
    var g fn(Point) Point = add_point;
    var p Point = f();
    var q Point = g(p);
    return q.x + q.y;
}`, false, 10, false)
}

// TestEmitFunctionTypeStrResultCompileAndRun is the exact F5-21 repro: a str
// result from a first-class function value. The function is called indirectly
// through a function-typed local, and the str result is bound to a str-typed
// local. The exit code 1 confirms the string comparison succeeded.
func TestEmitFunctionTypeStrResultCompileAndRun(t *testing.T) {
	t.Parallel()
	emitAndRun(t, `fn make_greeting() str {
    return "hello";
}

fn main() int {
    var f fn() str = make_greeting;
    var s str = f();
    if s == "hello" {
        return 1;
    }
    return 0;
}`, false, 1, false)
}
