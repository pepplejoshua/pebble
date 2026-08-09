package backend

import (
	"bytes"
	"strconv"
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

func TestEmitStdMemNewSliceCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdMemFixture(t, `import "std:mem"; fn main() i32 { var values []i32 = mem::new_slice[i32](3); values[0] = 42; return values[0]; }`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitSliceFromRawCompilesAndRuns(t *testing.T) {
	unit, snapshot, entryID, sources := buildStdFixture(t, "fn main() i32 { var value i32 = 42; var ptr *i32 = &value; let values []i32 = slice ptr, 1; return values[0]; }", "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitRuntimeAllocatorValueCompiles(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; return 0; }", 0)
}

func TestEmitRuntimeAllocatorStateFieldCompiles(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; let p = a.ptr; return 0; }", 0)
}

func TestEmitRuntimeAllocatorRoundTrip(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; var p *i32 = (a.alloc)(a.ptr, 4) as *i32; *p = 42; let value = *p; (a.free)(a.ptr, p as *void); return value; }", 42)
}

func TestEmitRuntimeAllocatorUnparenthesizedRoundTrip(t *testing.T) {
	emitRuntimeAndRun(t, "fn main() i32 { let a = context.default_allocator; var p *i32 = a.alloc(a.ptr, 4) as *i32; *p = 42; let value = *p; a.free(a.ptr, p as *void); return value; }", 42)
}

func TestEmitStructWithAllocatorFieldCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the runtime-builtin struct-field-typedef
	// gap: a struct with an Allocator-typed field (mirroring std/hmap.peb's
	// HashMap[K,V].backing Allocator), constructed from context.default_allocator
	// exactly as hmap's new/with_capacity do. Allocator is a compiler builtin
	// with a hand-written PebbleAllocator C type (never a per-TypeID
	// pebble_struct_<id>_t typedef), so orderAggregateTypes must skip it
	// entirely from the typedef-emission postorder while the struct's OWN real
	// fields still get typedefs; the Allocator field itself is declared with
	// PebbleAllocator and initialized from the runtime context. Reading the
	// Allocator field back into a local (`let a = h.backing`) exercises the
	// runtime-typed field-read path too. The program must compile and run.
	emitAndRun(t, "type Holder = struct { value int; backing Allocator; }; fn main() int { var h Holder = Holder.{ value = 41, backing = context.default_allocator }; let a = h.backing; return h.value + 1; }", false, 42, false)
}

func TestEmitStructWithAllocatorFieldWritesC(t *testing.T) {
	// Confirm the emitted C directly: the struct's own typedef carries its real
	// field AND the Allocator field declared as the hand-written PebbleAllocator
	// (NOT a per-TypeID pebble_struct_<id>_t, whose typedef orderAggregateTypes
	// now skips), the construction initializes it from the runtime context's
	// default allocator, and exactly ONE struct typedef is emitted — no typedef
	// for the Allocator type itself. This is the "skipped entirely, not a
	// zero-valued placeholder" behavior the orderAggregateTypes fix guarantees.
	unit, snapshot, entryID, sources := buildFixture(t, "type Holder = struct { value int; backing Allocator; }; fn main() int { var h Holder = Holder.{ value = 41, backing = context.default_allocator }; return h.value + 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleAllocator pebble_field_",
		"(*ctx).allocator",
		"int32_t pebble_field_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Count(out, "typedef struct {") != 1 {
		t.Errorf("emitted C must contain exactly one struct typedef (for Holder, not the skipped Allocator runtime type):\n%s", out)
	}
	openIdx := strings.Index(out, "typedef struct {")
	closeIdx := strings.Index(out[openIdx:], "} pebble_struct_")
	block := out[openIdx : openIdx+closeIdx]
	for _, line := range strings.Split(block, "\n") {
		if strings.Contains(line, "pebble_field_") && strings.Contains(line, "pebble_struct_") {
			t.Errorf("emitted C declares a struct field with a per-TypeID pebble_struct_ type; the Allocator field must use PebbleAllocator:\n%s", out)
		}
	}
}

func TestEmitRuntimeAllocatorRecordConstructCompilesAndRuns(t *testing.T) {
	// The exact Slice 1 boundary reproduction, now closed: constructing an
	// Allocator literal (Allocator.{ ptr, alloc, realloc, free }) from source
	// is a RecordConstruct whose Type is the nominal Allocator runtime type,
	// which has no parsed TypeDeclaration (the old "has no TypeDeclaration"
	// struct-collection failure, then the "declares a runtime-typed local
	// initialized from a RecordConstruct" runtime-local rejection). The record
	// is now emitted as a designated-initializer PebbleAllocator local whose
	// callback fields reference file-scope C bridges into the runtime callback
	// ABI. The program must compile and run.
	emitAndRun(t, `fn my_alloc(ctx *void, size uint) *void { return nil; }
fn my_realloc(ctx *void, ptr *void, size uint) *void { return nil; }
fn my_free(ctx *void, ptr *void) void {}
fn main() int {
    var a = Allocator.{
        ptr = nil,
        alloc = my_alloc,
        realloc = my_realloc,
        free = my_free,
    };
    return 0;
}`, false, 0, false)
}

func TestEmitRuntimeAllocatorRecordConstructWritesC(t *testing.T) {
	// Confirm the emitted C directly: the Allocator literal lowers to a
	// PebbleAllocator local declared with a C99 designated-initializer brace
	// list over the hand-written runtime struct, the ptr field maps to .state,
	// each callback field names a file-scope bridge (pebble_rt_alloc_adapter_<sym>
	// / pebble_rt_realloc_adapter_<sym> / pebble_rt_free_adapter_<sym>) with the
	// exact runtime callback ABI (hidden PebbleContext *ctx first parameter,
	// size_t sizes) rather than a function-pointer cast (which clang's
	// -Wcast-function-type-mismatch rejects under the -Wall -Wextra -Werror
	// build), and the bridges' definitions are emitted.
	unit, snapshot, entryID, sources := buildFixture(t, `fn my_alloc(ctx *void, size uint) *void { return nil; }
fn my_realloc(ctx *void, ptr *void, size uint) *void { return nil; }
fn my_free(ctx *void, ptr *void) void {}
fn main() int {
    var a = Allocator.{
        ptr = nil,
        alloc = my_alloc,
        realloc = my_realloc,
        free = my_free,
    };
    return 0;
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleAllocator pebble_local_",
		".state = ",
		".alloc = pebble_rt_alloc_adapter_",
		".realloc = pebble_rt_realloc_adapter_",
		".free = pebble_rt_free_adapter_",
		"static void *pebble_rt_alloc_adapter_",
		"static void *pebble_rt_realloc_adapter_",
		"static void pebble_rt_free_adapter_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitRuntimeAllocatorRecordConstructInitializerNotIgnored(t *testing.T) {
	// Prove the ptr initializer is actually stored, not zeroed: the literal
	// stores (&x) as *void into the .state field, reading the field back into a
	// pointer local and dereferencing it must yield x's value. A construction
	// whose .state initializer were ignored would leave state NULL and the
	// dereference would not return 42.
	emitAndRun(t, `fn my_alloc(ctx *void, size uint) *void { return nil; }
fn my_realloc(ctx *void, ptr *void, size uint) *void { return nil; }
fn my_free(ctx *void, ptr *void) void {}
fn main() int {
    var x = 42;
    var a = Allocator.{
        ptr = (&x) as *void,
        alloc = my_alloc,
        realloc = my_realloc,
        free = my_free,
    };
    var p *int = a.ptr as *int;
    return *p;
}`, false, 42, false)
}

func TestEmitRuntimeAllocatorRecordConstructCallbackInvoked(t *testing.T) {
	// Prove the alloc callback initializer is actually stored and invoked, not
	// ignored: the literal stores my_alloc (which returns its first argument,
	// the allocator's own state pointer) in the .alloc field, and calling it
	// through the existing allocator call lowering must route through the
	// emitted bridge into my_alloc and return the non-nil state. If the .alloc
	// initializer were ignored (a zeroed or default field), the call would not
	// return the non-nil state and the nil comparison would not pass.
	emitAndRun(t, `fn my_alloc(ctx *void, size uint) *void { return ctx; }
fn my_realloc(ctx *void, ptr *void, size uint) *void { return nil; }
fn my_free(ctx *void, ptr *void) void {}
fn main() int {
    var x = 42;
    var a = Allocator.{
        ptr = (&x) as *void,
        alloc = my_alloc,
        realloc = my_realloc,
        free = my_free,
    };
    var p *void = (a.alloc)(a.ptr, 4);
    if p != nil { return 42; }
    return 0;
}`, false, 42, false)
}

func TestEmitStructWithUintFieldCompilesAndRuns(t *testing.T) {
	// An ordinary (non-generic) struct field typed as uint -- the same narrow
	// entry-width/bool-only gate already widened this session for optional
	// payloads (d737242), slice elements (f85b4a0), and function-type
	// parameters/results (b5c139c), now widened for structFieldCType's own
	// scalar field-type gate: structFieldCType (the typedef), buildStructBraceList's
	// field-construction case, and buildStructFieldRead all gained a uint case.
	// Confirmed via the real std/hmap.peb motivating shape: HashMap's own
	// len/cap uint fields (see TestEmitStdHmapInsertGetFullConsumer).
	emitAndRun(t, "type Counter = struct { n uint; }; fn main() i32 { var c Counter = Counter.{ n = 5 }; return c.n as i32; }", false, 5, false)
}

func TestEmitStructEnumFieldCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the enum-typed-struct-field gap: a
	// struct with a plain-enum-typed field mirroring std/hmap.peb's Entry
	// exactly (type EntryState = enum { Empty, Tombstone, Occupied }; struct
	// Entry { key, value, state EntryState }). All four required operations
	// are exercised through a real program whose exit code depends on each
	// one working: CONSTRUCTION places the variant's C enum constant into the
	// field's own pebble_enum_<typeID>_t (state starts .Empty); a FIELD READ
	// (e.state == .Empty) compares the projected field directly against a
	// variant literal and gates the pointer write; a FIELD ASSIGNMENT
	// mutating the already-constructed instance (e.state = .Tombstone) fires
	// only if the pointer write took; and a write THROUGH a POINTER to the
	// struct (mutate(&e)'s e->state = .Occupied, the slot.state = .Occupied
	// shape from hmap's insert) mutates through the -> projection. The final
	// != comparison returns 0 only if state really is Tombstone; any one of
	// the four breaking changes the exit code.
	emitAndRun(t, "type EntryState = enum { Empty, Tombstone, Occupied };\ntype Entry = struct { key i32; value i32; state EntryState; };\nfn mutate(e *Entry) void { e.state = .Occupied; }\nfn main() i32 {\nvar e Entry = Entry.{ key = 1, value = 2, state = .Empty };\nif e.state == .Empty { mutate(&e); }\nif e.state == .Occupied { e.state = .Tombstone; }\nif e.state != .Tombstone { return 1; }\nreturn 0;\n}", false, 0, false)
}

func TestEmitStructEnumFieldWritesC(t *testing.T) {
	// Emitted-C shape check: the struct field whose type is a plain enum is
	// declared with the enum's OWN typedef name — pebble_enum_<typeID>_t, the
	// identical C type an enum-typed local/parameter/result uses (see
	// enumTypeName) — no new typedef machinery. The construction initializes
	// the field from the variant's C enum constant, the enum typedef itself is
	// still emitted, and the read/compare/assign all use the plain
	// pebble_field_<member> projection.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type EntryState = enum { Empty, Tombstone, Occupied };\ntype Entry = struct { key i32; value i32; state EntryState; };\nfn main() i32 {\nvar e Entry = Entry.{ key = 1, value = 2, state = .Empty };\nif e.state == .Empty { return 1; }\nreturn 0;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef enum {",
		enumTypeName(enumType) + " pebble_field_",
		"= pebble_variant_",
		".pebble_field_",
		" == pebble_variant_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitOptionalHasValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn main() i32 { let present ?i32 = some 7; if present.has_value { return 1; } else { return 0; } }`, false, 1, false)
	emitAndRun(t, `fn main() i32 { let absent ?i32 = none; if !absent.has_value { return 1; } else { return 0; } }`, false, 1, false)
}

func TestEmitFieldNilAssignmentRoundTripCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `type P = struct { d *i32; }; fn main() i32 { var value i32 = 7; var p P = P.{ d = &value }; p.d = nil; if p.d == nil { return 1; } else { return 0; } }`, false, 1, false)
}

func TestEmitGenericStructDataFieldsCompileAndRun(t *testing.T) {
	// Generic struct data fields slice 1: a generic struct whose fields are the
	// struct's own type parameters (`key K`, `value V`) must emit per
	// -specialization concrete field types, so Pair[int, int] constructs and
	// reads both fields back. Before the slice, MemberTypes[i] for `key K` /
	// `value V` was unresolved (the generic declaration's template is a type
	// parameter, never a TemplateKnown concrete type), so resolveStructInfo
	// had no concrete type and the typedef/field read failed. 5 * 10 + 10 = 60
	// encodes both field values in the exit code, proving key == 5 and
	// value == 10 independently.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; return p.key * 10 + p.value; }`, false, 60, false)
}

func TestEmitGenericStructTwoSpecializationsCompileAndRun(t *testing.T) {
	// Two specializations of the SAME generic struct in one program must emit
	// two DISTINCT, independently-correct C typedefs (pebble_struct_<typeID>_t
	// per specialization TypeID): Pair[int, int]'s value field is int while
	// Pair[int, bool]'s is bool. The two share every field symbol (the member
	// types are the same declaration), so the backend must resolve each
	// instantiation's field type from ITS OWN construction evidence, not a
	// per-field-symbol global map. The read of q.value must dispatch to the
	// bool grammar and the three width field reads to the integer grammar;
	// 5 + 10 + 6 = 21 confirms all four values end to end.
	emitAndRun(t, `type Pair[K, V] = struct { key K; value V; }; fn main() int { let p Pair[int, int] = Pair[int, int].{ key = 5, value = 10 }; let q Pair[int, bool] = Pair[int, bool].{ key = 6, value = true }; if q.value { return p.key + p.value + q.key; } else { return 0; } }`, false, 21, false)
}

func TestEmitGenericStructOptionalFieldSingleCompileAndRun(t *testing.T) {
	// Generic struct compound fields, optional-wrapped: a single
	// specialization of `Box[K] = struct { value ?K; }` with K = int must
	// compile AND run with the correct unwrapped value. The field's member
	// template wraps the struct's own parameter, so the checker leaves its
	// MemberTypes entry unresolved and the backend must recover the concrete
	// Optional(int) type from the construction evidence. 5 is the unwrapped
	// value.
	emitAndRun(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; return b.value!; }`, false, 5, false)
}

func TestEmitGenericStructOptionalTwoSpecializationsCompileAndRun(t *testing.T) {
	// The exact repro that was broken before this slice: TWO specializations
	// of the same generic struct in one program, the field type a compound
	// wrapping the struct's own parameter. Before the fix, Box[bool]'s struct
	// typedef reused Box[int]'s Optional(int) field type (pebble_optional_30_t)
	// and the Box[bool] construction referenced pebble_optional_31_t which was
	// never defined — a real cc failure (undeclared identifier). The
	// specialization is now resolved from its own construction evidence, so
	// each struct typedef carries its own payload optional and every optional
	// typedef is emitted. The three-way test proves all three values
	// independently: c.value! is true, d.value! is false (both bool
	// specializations, distinguishing by VALUE not just by type), b.value! is
	// 5 (int specialization), so the exit code 5 requires every field read to
	// dispatch against its own specialization's optional payload type. (d's
	// payload is `some false`, not `none` — force-unwrapping a `none` value
	// with `!` is checked and panics by design, so `none` can't stand in for
	// "false" here without aborting the program.)
	emitAndRun(t, `type Box[K] = struct { value ?K; }; fn main() int { var b Box[int] = Box[int].{ value = some 5 }; var c Box[bool] = Box[bool].{ value = some true }; var d Box[bool] = Box[bool].{ value = some false }; if c.value! { if d.value! { return 1; } else { return b.value!; } } else { return 0; } }`, false, 5, false)
}

func TestEmitGenericStructPointerFieldSingleCompileAndRun(t *testing.T) {
	// Generic struct compound fields, pointer-wrapped: a single specialization
	// of `Ref[K] = struct { ptr *K; }` with K = int must compile AND run with
	// the correct dereferenced value. The member's template wraps the struct's
	// own parameter, so its MemberTypes entry is unresolved and the backend
	// recovers the concrete *int pointee from the construction evidence; the
	// nil construction, the non-nil assignment, and the checked dereference all
	// agree on int32_t. 7 is the value read through the pointer field.
	emitAndRun(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var x int = 7; var p *int = &x; r.ptr = p; if r.ptr == nil { return 0; } else { return *r.ptr; } }`, false, 7, false)
}

func TestEmitGenericStructPointerTwoSpecializationsCompileAndRun(t *testing.T) {
	// The pointer analogue of the two-specialization repro: before this slice,
	// Ref[bool]'s struct typedef field reused Ref[int]'s pointee (both declared
	// `bool *` or both `int32_t *` depending on node order) — silently wrong
	// and a -Werror incompatible-pointer-types failure under the runtime
	// Makefile's flags. Now each specialization's pointee is recovered from its
	// own construction evidence. The program proves both independently: *s.ptr
	// reads Ref[bool]'s bool true (driving the outer if), *r.ptr reads Ref[int]'s
	// int 7 (the returned value), so exit code 7 requires both field reads to
	// dispatch against their own specialization's pointee type.
	emitAndRun(t, `type Ref[K] = struct { ptr *K; }; fn main() int { var r Ref[int] = Ref[int].{ ptr = nil }; var s Ref[bool] = Ref[bool].{ ptr = nil }; var x int = 7; var y bool = true; var p *int = &x; var q *bool = &y; r.ptr = p; s.ptr = q; if s.ptr == nil { return 0; } else { if *s.ptr { return *r.ptr; } else { return 1; } } }`, false, 7, false)
}

func TestEmitGenericStructNestedFieldCompileAndRun(t *testing.T) {
	// Nested-generic struct fields: a generic struct field typed as ANOTHER
	// generic struct instantiated with the outer's own parameter
	// (`Outer[K] = struct { inner Inner[K]; }`). The inner construction
	// (`Inner[int].{ val = 5 }`) lives at the outer RecordConstruct's
	// Fields[0].Value — reachable only via the field-value recursion this
	// slice adds to collectStructTypesWalk, never via the Children-following
	// recursion (field values are in node.Fields, not node.Children, the same
	// gap collectFunctionTypesWalk/collectOptionalTypesWalk already closed) —
	// so before the fix the inner struct type was never collected for a C
	// typedef and emission failed outright with "struct type 0 is not in the
	// type snapshot" (the outer field read's unresolved TypeID). 5 is the
	// value read through both struct layers.
	emitAndRun(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; return o.inner.val; }`, false, 5, false)
}

func TestEmitGenericStructNestedFieldTwoSpecializationsCompileAndRun(t *testing.T) {
	// Two specializations of the outer struct in one program, each carrying a
	// DIFFERENT nested specialization (`Inner[int]` vs `Inner[bool]`): every
	// field symbol is shared across specializations (they come from the same
	// declaration), but each instantiation's nested struct type is now
	// collected from its own construction subtree, so Outer[int]'s inner and
	// Outer[bool]'s inner resolve to distinct C struct typedefs with the
	// correct per-K payload. b.inner.val must dispatch against Inner[bool]'s
	// bool grammar and o.inner.val against Inner[int]'s int grammar; exit
	// code 5 requires both field reads to hit their own specialization.
	emitAndRun(t, `type Inner[T] = struct { val T; }; type Outer[K] = struct { inner Inner[K]; }; fn main() int { var o Outer[int] = Outer[int].{ inner = Inner[int].{ val = 5 } }; var b Outer[bool] = Outer[bool].{ inner = Inner[bool].{ val = true } }; if b.inner.val { return o.inner.val; } else { return 0; } }`, false, 5, false)
}

func TestEmitGenericStructDataFieldsCrossModuleContextCompileAndRun(t *testing.T) {
	// Generic struct data fields slice 1 (254a00c) substituted a directly
	// parameter-typed field from the struct's generic declaration key in the
	// snapshot — but only when that declaration key was the FIRST
	// all-TypeParameter-arg Nominal key for the declaration in allocation
	// order. A generic struct referenced from another generic context (a
	// method, or a generic function that names the struct with ITS OWN type
	// parameters) interns a SEPARATE all-TypeParameter key per context, so a
	// plain first-match scan could return a function/method context's key —
	// whose parameter symbols never match the declaration's own field
	// MemberTypes (each context's parameters are distinct symbols) — leaving
	// `val K` unsubstituted and buildStructTypedef rejecting the
	// type-parameter field. This is the exact std/hmap.peb Entry[int,int]
	// root cause: HashMap's entries []Entry[K,V] field reaches Entry's key K /
	// value V, and hmap's eight methods each intern an Entry[K,V] key with
	// their own inherited K/V symbols before Entry's canonical key lands.
	// This fixture is the minimal repro: Inner[K] (a direct parameter field
	// plus a method, which adds another all-TypeParameter key), Outer[K]
	// carrying an Inner[K] field, and a generic function newo[K] that
	// constructs Outer[K] with newo's OWN K — the function-context key interns
	// before Inner's canonical key. Before structTypeParameters selected the
	// canonical key (whose parameters are the ones the declaration's own field
	// MemberTypes reference), Emit failed with "field type type-parameter(...)
	// is not supported"; now the emitted Inner[int] typedef carries the
	// concrete int32_t field and the program compiles and runs to 5.
	sources := source.NewFileSet()
	diagnostics := diagnostic.NewDiagnosticSet()
	provider := fixtureProvider{
		"main.peb": []byte(`import "std:lib"; fn main() int { var o = lib::newo[int](5); return o.inner.val; }`),
		"std/lib.peb": []byte(`type Inner[K] = struct {
    val K;
    fn bump[K](self *Inner[K]) void { self.val = self.val; }
};
type Outer[K] = struct {
    inner Inner[K];
    fn touch[K](self *Outer[K]) void { self.inner.val = self.inner.val; }
};
fn newo[K](v K) Outer[K] {
    return Outer[K].{ inner = Inner[K].{ val = v } };
}`),
	}
	graph := module.Build(module.BuildConfig{EntryPath: "main.peb", Package: "app", StandardRoot: "std"}, provider, sources, diagnostics)
	resolution := symbol.Resolve(graph, sources, diagnostics, symbol.Config{})
	store, err := types.New(types.Config{})
	if err != nil {
		t.Fatal(err)
	}
	result := check.Check(check.Inputs{Graph: graph, Sources: sources, Resolution: resolution, Types: store, LiteralTarget: infer.LiteralTarget{WordBits: 64}}, diagnostics, check.Config{})
	if !result.Successful() {
		t.Fatalf("check failed: %+v", diagnostics.Items())
	}
	unit := result.IR()
	if unit == nil {
		t.Fatal("check succeeded without an IR unit")
	}
	var entryID symbol.SymbolID
	for _, candidate := range resolution.Symbols.All() {
		if candidate.Name == "main" {
			entryID = candidate.ID
		}
	}
	if entryID == 0 {
		t.Fatal("missing symbol \"main\"")
	}
	var buf bytes.Buffer
	if err := Emit(unit, unit.Snapshot(), entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed on the cross-module generic struct data-field fixture; structTypeParameters must substitute the declaration's own parameters, not a generic-function context's: %v", err)
	}
	out := buf.String()
	if strings.Contains(out, "type-parameter") {
		t.Errorf("emitted C still carries an unsubstituted type-parameter field:\n%s", out)
	}
	// The Inner[int] typedef's val field must be the concrete int32_t (the
	// shape that failed before the fix).
	if !strings.Contains(out, "    int32_t pebble_field_") {
		t.Errorf("emitted C missing Inner[int]'s concrete int32_t val field:\n%s", out)
	}
	// The end-to-end proof: reading o.inner.val through both struct layers
	// must compile under -Wall -Wextra -Werror and exit 5.
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitFixedStrArrayLocalDeclarationCompilesAndRuns(t *testing.T) {
	// Fixed str arrays are supported for array-literal local declarations.
	emitAndRun(t, `fn main() i32 { let values [2]str = ["first", "second"]; return 0; }`, false, 0, false)
}

func TestEmitFixedStrArrayElementReadInComparisonCompilesAndRuns(t *testing.T) {
	// The missing Load(CheckedIndexPlace) case in buildStrOperand: the checker
	// lowers values[i] for a fixed [N]str local to a str-typed Load whose place
	// is a CheckedIndexPlace, and the backend now emits the read through
	// buildArrayPlaceRead as pebble_local_<sym>[pebble_rt_checked_index_...]
	// — the C subscript yields the array's PebbleStr element directly, so it
	// feeds a str comparison operand without any coercion. values[0] is "hi",
	// so the comparison holds and the process exits 7 (else 3).
	emitAndRun(t, `fn main() i32 { let values [2]str = ["hi", "ho"]; if values[0] == "hi" { return 7; } else { return 3; } }`, false, 7, false)
}

func TestEmitFixedStrArrayElementReadAsCallArgumentCompilesAndRuns(t *testing.T) {
	// The same fixed [N]str indexed read used as a call-site argument (the
	// exact std/hmap.peb insert shape, which passes a str element into a
	// str-taking helper): the argument is built by buildStrOperand, which now
	// accepts the Load(CheckedIndexPlace) of the str element. The helper
	// returns a == b, so passing values[1] ("ho") and an equal literal exits 7
	// (else 3).
	emitAndRun(t, "fn eq(a str, b str) bool { return a == b; }\nfn main() i32 { let values [2]str = [\"hi\", \"ho\"]; if eq(values[1], \"ho\") { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitCompoundSliceElementCompilesAndRuns(t *testing.T) {
	// A compound assignment to a slice element — s[0] += 4 — is a
	// CompoundStore whose place is a CheckedIndexPlace over a slice base,
	// emitted as s.data[pebble_rt_checked_index_i32(...)] both read and
	// written. The write lands in the backing array, so values[0] goes 1 -> 5.
	emitAndRun(t, "fn main() i32 { var values [3]i32 = [1, 2, 3]; let s []i32 = values[:]; s[0] += 4; return values[0]; }", false, 5, false)
}

func TestEmitRangeLoopArrayIndexCompilesAndRuns(t *testing.T) {
	// The iterator used as an array index (`a[i]`): the int-typed iterator
	// SymbolValue is lowered as its pebble_local_<symbol> name by the
	// array-index int-typed-SymbolValue case, so the sum of the three elements
	// (10+20+30) = 60 is returned. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; var sum i32 = 0; loop 0..3 : i { sum = sum + a[i]; } return sum; }", false, 60, false)
}

func TestEmitArrayHelperParameterAndResultCompilesAndRuns(t *testing.T) {
	output := emitAndRunCaptureBounded(t, `fn sort_once(values [5]int) [5]int {
    if values[0] > values[1] {
        let first = values[0];
        values[0] = values[1];
        values[1] = first;
    }
    return values;
}
fn main() int {
    var values [5]int = [2, 1, 3, 4, 5];
    let sorted = sort_once(values);
    print sorted[0];
    print sorted[1];
    return 0;
}`, false, 0, false)
	if output != "1\n2\n" {
		t.Fatalf("array helper output = %q, want %q", output, "1\n2\n")
	}
}

func TestEmitArrayHelperLiteralArgumentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn f(a [3]i32) i32 { return 1; } fn main() i32 { return f([10, 20, 30]); }", false, 1, false)
}

func TestEmitTupleTwoElementReadBackCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for tuple construction and an element read: a
	// two-element (i32, i32) tuple is declared from a tuple literal and its
	// second element is read back into the return value. The tuple type emits
	// one struct typedef with fields _0/_1 and the local is initialized with
	// the struct literal { 20, 22 }, and the element read lowers to
	// pebble_local_<id>._1, so the process exit code is 22.
	emitAndRun(t, "fn main() i32 { let t (i32, i32) = (20, 22); return t.1; }", false, 22, false)
}

func TestEmitTupleElementsReadBackAndAddedCompilesAndRuns(t *testing.T) {
	// The "elements read back and added" fixture: a three-element tuple's
	// elements 1 and 2 are read back and added: t.1 + t.2 = 20 + 30 = 50.
	emitAndRun(t, "fn main() i32 { let t (i32, i32, i32) = (10, 20, 30); return t.1 + t.2; }", false, 50, false)
}

func TestEmitTupleElementZeroReadCompilesAndRuns(t *testing.T) {
	// Regression test: reading a tuple's element 0 (t.0) used to be impossible
	// from any source — the tir verifier rejected a TuplePlace/TupleElementValue
	// with Ordinal 0, because Node.Ordinal is a zero-based element index (0 is
	// the tuple's first element, a legitimate value) but the verifier treated
	// Ordinal == 0 as an absent-field sentinel. Fixed directly in
	// compiler/internal/tir/verify.go (not this package): the erroneous
	// "requires Ordinal" checks for TupleElementValue/TuplePlace were removed,
	// since 0 is not damage and there is no way to distinguish a genuinely
	// unset Ordinal from a legitimate element-0 index without a type-level
	// cross-check the structural verifier doesn't otherwise do. t.0 now reads
	// the tuple's first element: pebble_local_<id>._0 = 20.
	emitAndRun(t, "fn main() i32 { let t (i32, i32) = (20, 22); return t.0; }", false, 20, false)
}

func TestEmitTupleThreeElementWritesC(t *testing.T) {
	// The emitted C for the three-element tuple: one struct typedef with three
	// positional fields written before pebble_user_main (definition before
	// use), the local declared as the typedef type and initialized with the
	// brace literal { 10, 20, 30 }, and both element reads lowering to
	// pebble_local_<id>._1 / ._2 inside the checked add. Symbol 25 is the t
	// local and tuple type 23 its (i32, i32, i32) type, confirmed against the
	// real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let t (i32, i32, i32) = (10, 20, 30); return t.1 + t.2; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n    int32_t _2;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_27 = { 10, 20, 30 };",
		"    (void)pebble_local_27;",
		"return pebble_rt_checked_add_i32(pebble_local_27._1, pebble_local_27._2, (PebbleSourceLoc){\"main.peb\", 1, 62});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("tuple typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitTupleBoolElementDrivesIfCompilesAndRuns(t *testing.T) {
	// A tuple with a bool element mixed beside an integer element: the bool
	// element read (t.1) drives an if condition. t.1 = true runs the then-arm
	// (exit 10); with the bool element false the else-arm runs (exit 20). The
	// read is a bool value, so it must route through the bool grammar
	// (buildBoolExpr's Load case), not the integer one.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "fn main() i32 { let t (i32, bool) = (1, true); if t.1 { return 10; } else { return 20; } }", 10},
		{"bool false", "fn main() i32 { let t (i32, bool) = (1, false); if t.1 { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitTupleBoolElementDrivesIfWritesC(t *testing.T) {
	// The emitted C for the bool-element-if fixture: the typedef's second field
	// must be the C bool, the local's initializer carries the integer and bool
	// literals in order, and the if condition is the raw field read
	// pebble_local_<id>._1 (a C bool needs no comparison). Symbol 25 is the t
	// local, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let t (i32, bool) = (1, true); if t.1 { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    bool _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_27 = { 1, true };",
		"    if (pebble_local_27._1) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !strings.Contains(out, "return 10;") || !strings.Contains(out, "return 20;") {
		t.Errorf("emitted C missing the if/else arms:\n%s", out)
	}
}

func TestEmitTupleElementAsCallArgumentCompilesAndRuns(t *testing.T) {
	// A tuple element read composes with 10.18's call-argument building: each
	// argument of add is a read of the tuple local's element 1. buildCallArguments
	// builds each argument with buildExpr, which lowers the Load(TuplePlace) read
	// to pebble_local_<id>._1, so add(22, 22) = 44 is the process exit code.
	// The tuple typedef must still be emitted before the helper's definition.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let t (i32, i32) = (20, 22); return add(t.1, t.1); }", false, 44, false)
}

func TestEmitTupleElementAsCallArgumentWritesC(t *testing.T) {
	// The emitted C for the call-argument fixture: the tuple typedef precedes
	// both the helper and the entry, the local initializes to { 20, 22 }, and
	// the call site passes the two element reads after ctx. Symbols 24 (add),
	// 25/26 (its parameters), and 28 (t) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let t (i32, i32) = (20, 22); return add(t.1, t.1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_30 = { 20, 22 };",
		"return pebble_fn_24(ctx, pebble_local_30._1, pebble_local_30._1);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// A tuple-typed local declared inside a reachable helper's body, not the
	// entry's: the typedef-collection pass must walk helper bodies too, so the
	// tuple typedef is emitted before the helper's definition (which is before
	// pebble_user_main). helper declares t, reads its element 1, and returns
	// it; the entry just calls helper, so exit code 22 proves the helper's
	// tuple local was built and the typedef emitted correctly.
	emitAndRun(t, "fn helper() i32 { let t (i32, i32) = (20, 22); return t.1; } fn main() i32 { return helper(); }", false, 22, false)
}

func TestEmitI64TupleCompilesAndRuns(t *testing.T) {
	// The width discipline extends to tuple element types: an i64 entry's
	// (i64, i64) tuple's typedef fields are int64_t, the local is int64_t
	// backed, and the element read feeds the i64 entry's return. Exit code 22.
	emitAndRun(t, "fn main() i64 { let t (i64, i64) = (20, 22); return t.1; }", false, 22, false)
}

func TestEmitI64TupleWritesC(t *testing.T) {
	// The emitted C for the i64 tuple must use int64_t for both typedef fields
	// and for the pebble_user_main return type, proving the entry's width
	// threads into the tuple layout, not just the scalar declarations.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let t (i64, i64) = (20, 22); return t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int64_t _0;\n    int64_t _1;\n} pebble_tuple_23_t;",
		"pebble_tuple_23_t pebble_local_27 = { 20, 22 };",
		"return pebble_local_27._1;",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t _0") {
		t.Errorf("emitted C declared an i32 tuple field for an i64 entry:\n%s", out)
	}
}

func TestEmitArrayElementReadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[1]; }", false, 20, false)
}

func TestEmitArrayBoolElementDrivesIf(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [2]bool = [false, true]; if a[1] { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitArrayExpressionIndexCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; let i i32 = 1; return a[i + 1]; }", false, 30, false)
}

func TestEmitArrayOutOfBoundsAborts(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let a [2]i32 = [10, 20]; let i i32 = 2; return a[i]; }", false, 0, true)
}

func TestEmitIntEntryArrayElementReadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() int { let a [3]int = [10, 20, 30]; return a[1]; }", true, 20, false)
}

func TestEmitIntEntryArrayOutOfBoundsAborts(t *testing.T) {
	emitAndRun(t, "fn main() int { let a [2]int = [10, 20]; let i int = 2; return a[i]; }", true, 0, true)
}

func TestEmitCheckedArrayIndexEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, checked array indexing carries a real, resolved Pebble
	// source location (the CheckedIndexPlace node's own Span) as its final
	// argument, not the zero-valued placeholder 10.43 deliberately left here
	// — this is one of the three checked-call categories 10.44 finished. The
	// fixture must still compile and run correctly end to end.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_index_i32(0, 3, (PebbleSourceLoc){"main.peb", 1,`) {
		t.Errorf("emitted C lacks a real source location on the checked-index call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 10, false)
}

func TestEmitArrayElementAsCallArgument(t *testing.T) {
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let a [2]i32 = [20, 22]; return add(a[0], a[1]); }", false, 42, false)
}

func TestEmitI64ArrayCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i64 { let a [2]i64 = [20, 22]; return a[1]; }", false, 22, false)
}

func TestEmitArrayWritesC(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [10, 20, 30]; return a[1]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_27[3] = { 10, 20, 30 };",
		"return pebble_local_27[pebble_rt_checked_index_i32(1, 3, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitArrayRepeatSumCompilesAndRuns(t *testing.T) {
	// The flagship ArrayRepeat fixture (10.27): [5; 3] initializes all three
	// slots from a single evaluation of 5, so the sum of every element read is
	// 5 + 5 + 5 = 15. This is the end-to-end confirmation that the
	// three-statement emission (bare declaration, one-time repeat temp, fill
	// loop) produces a correct array that reads back exactly like 10.20's
	// ArrayValue arrays.
	emitAndRun(t, "fn main() i32 { let a [3]i32 = [5; 3]; return a[0] + a[1] + a[2]; }", false, 15, false)
}

func TestEmitArrayRepeatExprValueCompilesAndRuns(t *testing.T) {
	// A repeat value that is itself a non-trivial expression: x * 5 references
	// an earlier local through checked arithmetic, so the single evaluation is
	// pebble_rt_checked_mul_i32(pebble_local_<x>, 5) = 10, and all three slots
	// get that one value: 10 + 10 + 10 = 30. Proves the repeat value is built
	// through buildExpr (a local reference composing with checked arithmetic),
	// not just a bare literal.
	emitAndRun(t, "fn main() i32 { let x i32 = 2; let a [3]i32 = [x * 5; 3]; return a[0] + a[1] + a[2]; }", false, 30, false)
}

func TestEmitArrayRepeatBoolElementDrivesIfCompilesAndRuns(t *testing.T) {
	// A bool-element array repeat: [true; 2] fills both bool slots from one
	// evaluation of true, and the element read drives an if condition through
	// the existing Load(CheckedIndexPlace) bool path. a[1] is true, so the
	// then-arm runs and the process exits 10.
	emitAndRun(t, "fn main() i32 { let a [2]bool = [true; 2]; if a[1] { return 10; } else { return 20; } }", false, 10, false)
}

func TestEmitArrayRepeatI64CompilesAndRuns(t *testing.T) {
	// The width discipline extends to array repeat element types: an i64
	// entry's [2]i64 repeat fills both int64_t slots, the reads lower through
	// pebble_rt_checked_index_i64, and 7 + 7 = 14 is the process exit code.
	emitAndRun(t, "fn main() i64 { let a [2]i64 = [7; 2]; return a[0] + a[1]; }", false, 14, false)
}

func TestEmitArrayRepeatWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: an ArrayRepeat-initialized local
	// is three C statements instead of one declaration line — the array's own
	// bare declaration, a synthetic temp (pebble_repeat_<symbolID>) holding
	// the one-time-evaluated repeat value, and a for loop over a size_t
	// counter (pebble_i_<symbolID>) filling every slot from the temp — still
	// followed by the (void) cast every array local gets. Both synthetic names
	// derive from the local's own symbol (25, confirmed against the real
	// fixture dump). The element reads are unchanged from 10.20.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a [3]i32 = [5; 3]; return a[0] + a[1] + a[2]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"    int32_t pebble_local_27[3];\n    int32_t pebble_repeat_27 = 5;\n    for (size_t pebble_i_27 = 0; pebble_i_27 < 3; pebble_i_27++) {\n        pebble_local_27[pebble_i_27] = pebble_repeat_27;\n    }\n    (void)pebble_local_27;",
		"pebble_rt_checked_index_i32(0, 3, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitArrayRepeatSingleEvaluationWritesC(t *testing.T) {
	// The single-evaluation proof: a repeat value that is a call to a helper
	// function. A naive brace-list duplication ({ v, v, v }) would have
	// emitted pebble_fn_<five>(ctx) three times — once per slot — evaluating
	// the call three times. This backend's one-time temp emission must contain
	// the call expression exactly once (in the pebble_repeat_<sym> = ... line),
	// so the call is evaluated exactly once at runtime. The assertion is
	// structural (strings.Count over the emitted C), the strongest proof
	// available without mutable global state to observe call count; the
	// end-to-end run confirms the resulting values are still correct
	// (5 + 5 + 5 = 15).
	unit, snapshot, entryID, sources := buildFixture(t, "fn five() i32 { return 5; } fn main() i32 { let a [3]i32 = [five(); 3]; return a[0] + a[1] + a[2]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// five's own body is `return 5;` (no call), so the only
	// pebble_fn_24(ctx) call site in the whole emitted file is the repeat
	// temp initializer — exactly one.
	if got := strings.Count(out, "pebble_fn_24(ctx)"); got != 1 {
		t.Errorf("pebble_fn_24(ctx) appears %d time(s) in the emitted C, want exactly 1 (the repeat value must be evaluated once, not once per slot):\n%s", got, out)
	}
	if !strings.Contains(out, "int32_t pebble_repeat_28 = pebble_fn_24(ctx);") {
		t.Errorf("emitted C missing the one-time repeat temp initialized from the call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 15, false)
}

func TestEmitNestedTupleElementCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn main() i32 { let inner (i32, i32) = (20, 22); let outer ((i32, i32), bool) = (inner, true); return (outer.0).1; }", false, 22, false)
}

func TestEmitTupleParameterCompilesAndRuns(t *testing.T) {
	// The flagship tuple-parameter fixture: sumT takes a whole (i32, i32)
	// tuple as its parameter and reads both elements back inside the callee,
	// including element 0 (confirming 10.19's tir-ordinal fix still holds in
	// the parameter path). The entry declares a tuple local and passes it by
	// value; the callee's parameter seeds its own scope as a tuple local, so
	// the reads resolve through the same Load(TuplePlace) machinery a tuple
	// local uses. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }", false, 42, false)
}

func TestEmitTupleParameterWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the tuple typedef must precede
	// the helper, the helper's signature declares the parameter as
	// pebble_tuple_<typeID>_t pebble_local_<paramSymbol> (symbol 25, the t
	// parameter), the parameter gets the same (void) cast, the body reads
	// pebble_local_25._0 / ._1, and the call site passes the entry's tuple
	// local pebble_local_27 directly (no construction at the call site).
	// Symbols 24 (sumT), 25 (t param), 26 (main), 27 (t local), and tuple type
	// 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn sumT(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { let t (i32, i32) = (20, 22); return sumT(t); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"static int32_t pebble_fn_24(PebbleContext *ctx, pebble_tuple_23_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"    return pebble_rt_checked_add_i32(pebble_local_25._0, pebble_local_25._1, (PebbleSourceLoc){\"main.peb\", 1, 36});",
		"pebble_tuple_23_t pebble_local_29 = { 20, 22 };",
		"return pebble_fn_24(ctx, pebble_local_29);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleParameterUsedInBoolElementAndSecondCall(t *testing.T) {
	// A tuple parameter whose element types are mixed width/bool: the callee
	// reads the bool element to drive an if and the i32 element as a value.
	// This proves the parameter's element reads route through buildBoolExpr
	// (the Load(TuplePlace) bool path) exactly as a tuple local's do.
	// choose((10, true)) takes the then-arm and returns the i32 element 10.
	emitAndRun(t, "fn choose(t (i32, bool)) i32 { if t.1 { return t.0; } else { return 99; } } fn main() i32 { let t (i32, bool) = (10, true); return choose(t); }", false, 10, false)
}

func TestEmitOptionalNoneNeverUnwrappedCompilesClean(t *testing.T) {
	// Regression coverage: `none` was initially thought unreachable from real
	// source (a since-fixed checker bug, compiler/internal/check's
	// shapeLeaf, made `let x ?i32 = none;` fail to type-check). It is
	// reachable — this proves a none-initialized local, never unwrapped,
	// compiles clean under -Wall -Wextra -Werror.
	emitAndRun(t, "fn main() i32 { let x ?i32 = none; return 1; }", false, 1, false)
}

func TestEmitOptionalUnwrapNoneAborts(t *testing.T) {
	// Force-unwrapping a none-initialized local panics via
	// pebble_rt_checked_unwrap_i32, aborting the process.
	emitAndRun(t, "fn main() i32 { let x ?i32 = none; return x!; }", false, 0, true)
}

func TestEmitOptionalUnwrapNoneEmitsRealSourceLoc(t *testing.T) {
	// The absent-optional unwrap still panics (the process aborts via
	// pebble_rt_checked_unwrap_i32), and — new for this slice — the emitted
	// unwrap call now carries the CheckedOptionalUnwrap node's own resolved
	// Pebble source location as its final argument, not the zero-valued
	// placeholder. The emitted C is inspected for the non-placeholder
	// compound literal, and the compiled binary is run to confirm the unwrap
	// of `none` still aborts.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?i32 = none; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, `pebble_rt_checked_unwrap_i32(pebble_local_27.has_value, pebble_local_27.value, (PebbleSourceLoc){"main.peb", 1, `) {
		t.Errorf("emitted C lacks a real source location on the checked-unwrap call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitOptionalSomeUnwrapCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for optional construction and force-unwrap: a
	// some <expr> local is declared and force-unwrapped as the return value.
	// The optional type emits one struct typedef with has_value/value fields,
	// the local is initialized with { .has_value = true, .value = 42 }, and
	// the force-unwrap lowers to pebble_rt_checked_unwrap_i32, so the process
	// exit code is 42.
	emitAndRun(t, "fn main() i32 { let x ?i32 = some 42; return x!; }", false, 42, false)
}

func TestEmitOptionalSomeUnwrapI64CompilesAndRuns(t *testing.T) {
	// The i64 width discipline extends to optional payload types: an i64
	// entry's ?i64 optional's typedef value field is int64_t, the local is
	// initialized with the i64 payload, and the force-unwrap lowers to
	// pebble_rt_checked_unwrap_i64. Exit code 22.
	emitAndRun(t, "fn main() i64 { let x ?i64 = some 22; return x!; }", false, 22, false)
}

func TestEmitOptionalBoolPayloadDrivesIfCompilesAndRuns(t *testing.T) {
	// A bool-payload optional, force-unwrapped to drive an if condition. The
	// unwrap lowers to pebble_rt_checked_unwrap_bool, whose result drives the
	// if condition directly. With the bool payload true the then-arm runs
	// (exit 10); with false the else-arm runs (exit 20).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "fn main() i32 { let x ?bool = some true; if x! { return 10; } else { return 20; } }", 10},
		{"bool false", "fn main() i32 { let x ?bool = some false; if x! { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitOptionalUnwrapAsCallArgumentCompilesAndRuns(t *testing.T) {
	// An unwrapped optional element used as a call argument: the force-unwrap
	// produces an ordinary i32 value that is passed to a helper function.
	// add(x!, y!) = 10 + 20 = 30 is the process exit code.
	emitAndRun(t, "fn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let x ?i32 = some 10; let y ?i32 = some 20; return add(x!, y!); }", false, 30, false)
}

func TestEmitOptionalSomeUnwrapWritesC(t *testing.T) {
	// The emitted C for the some-unwrap fixture: the optional typedef with
	// has_value/value fields, the local initialized with the struct literal
	// initializer, and the unwrap call site using pebble_rt_checked_unwrap_i32.
	// Symbol IDs come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?i32 = some 42; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int32_t value;\n} pebble_optional_",
		".has_value = true, .value = 42",
		"pebble_rt_checked_unwrap_i32(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("optional typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitOptionalI64WritesC(t *testing.T) {
	// The emitted C for the i64 optional must use int64_t for the typedef
	// value field and int64_t for pebble_user_main's return type, proving the
	// entry's width threads into the optional layout.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i64 { let x ?i64 = some 22; return x!; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int64_t value;\n} pebble_optional_",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t value;") {
		t.Errorf("emitted C declared an i32 optional value field for an i64 entry:\n%s", out)
	}
}

func TestEmitOptionalBoolWritesC(t *testing.T) {
	// The emitted C for the bool optional must use bool for the typedef value
	// field and the unwrap must use pebble_rt_checked_unwrap_bool.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let x ?bool = some true; if x! { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    bool value;\n} pebble_optional_",
		"pebble_rt_checked_unwrap_bool(",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitOptionalLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// An optional-typed local declared inside a reachable helper's body, not
	// the entry's: the typedef-collection pass must walk helper bodies too, so
	// the optional typedef is emitted before the helper's definition (which is
	// before pebble_user_main). helper declares x, force-unwraps it, and
	// returns it; the entry just calls helper, so exit code 42 proves the
	// helper's optional local was built and the typedef emitted correctly.
	emitAndRun(t, "fn helper() i32 { let x ?i32 = some 42; return x!; } fn main() i32 { return helper(); }", false, 42, false)
}

func TestEmitOptionalLocalStoreCompilesAndRuns(t *testing.T) {
	// Reassigning an optional-typed local (x = some 5) is now supported —
	// the std/hmap.peb insert shape (`tombstone_index = some index;`) needed
	// it, and buildStoreCore's optional-local case routes the new value
	// through buildOptionalValue, the same machinery an optional local's own
	// declaration initializer uses, emitting a plain whole-struct C
	// assignment. This was previously a clean rejection
	// ("reassigning an optional is not supported yet"); it is now a real
	// compile-and-run case: x is reassigned from some 1 to some 2, and the
	// force-unwrap returns 2.
	emitAndRun(t, "fn main() i32 { var x ?i32 = some 1; x = some 2; return x!; }", false, 2, false)
}

func TestEmitOptionalUintSomeUnwrapCompilesAndRuns(t *testing.T) {
	// The std/hmap.peb-motivating payload width: a uint-payload optional
	// constructed with some and force-unwrapped. The typedef's .value field is
	// uint64_t (uint resolves to its OWN C width via the generic
	// resolvedBuiltin/cType pattern), the payload is built through uint's
	// dedicated buildUintExpr grammar, and the force-unwrap routes to the new
	// pebble_rt_checked_unwrap_u64 runtime helper (uint's uint64_t needs a
	// uint64_t-width unwrap, not the entry-width i32 helper). 5 as int exits
	// 5.
	emitAndRun(t, "fn main() i32 { var o ?uint = some 5; return o! as i32; }", false, 5, false)
}

func TestEmitOptionalUintNoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a uint-payload optional: zeroOptionalPayloadLiteral
	// must pick a warning-clean zero literal for the uint64_t .value field (a
	// bare 0, scalar), the local initializes with has_value = false, and the
	// has_value read drives an if to the else arm. The process exit code 0
	// proves the none path — not the some path — was taken.
	emitAndRun(t, "fn main() i32 { var o ?uint = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalUintSomeHasValueCompilesAndRuns(t *testing.T) {
	// The same has_value check with a some-initialized uint optional, so the
	// true arm is taken: proves the has_value tag is set by the some
	// construction and read back correctly for a uint payload (exit 1).
	emitAndRun(t, "fn main() i32 { var o ?uint = some 9; if o.has_value { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitOptionalU64SomeUnwrapCompilesAndRuns(t *testing.T) {
	// u64 shares the uint64_t C representation (and thus the same generic
	// resolvedBuiltin/cType typedef and the same pebble_rt_checked_unwrap_u64
	// unwrap helper) as uint, but is a DISTINCT builtin that flows through the
	// general buildExpr path at its own width rather than buildUintExpr. Exit
	// 22.
	emitAndRun(t, "fn main() i32 { var o ?u64 = some 22; return o! as i32; }", false, 22, false)
}

func TestEmitOptionalU64NoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a u64-payload optional: the .value zero literal (0) and
	// the has_value=false tag both hold for a u64 payload.
	emitAndRun(t, "fn main() i32 { var o ?u64 = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalUintHmapInsertShapeCompilesAndRuns(t *testing.T) {
	// The real motivating fixture, mirroring std/hmap.peb's actual insert
	// shape without touching the std module: a none-initialized ?uint local
	// (tombstone_index), a has_value check, and a force-unwrap into a uint
	// binding inside the true arm. Since tombstone_index is none, the else
	// arm runs and the exit code is 3; the false-arm only is exercised
	// because that is what hmap's first insert does (tombstone_index starts
	// none).
	emitAndRun(t, "fn main() i32 { var tombstone_index ?uint = none; var result i32 = 0; if tombstone_index.has_value { let t = tombstone_index!; result = t as i32; } else { result = 3; } return result; }", false, 3, false)
}

func TestEmitOptionalUintHmapInsertShapeSomeCompilesAndRuns(t *testing.T) {
	// The same insert-shaped fixture with tombstone_index some-initialized, so
	// the has_value true arm runs: the force-unwrap binds t as a uint local
	// and its value (7) is cast to int and returned — the exact statement
	// sequence hmap.insert's tombstone path performs (`let t =
	// tombstone_index!; ... slot`). Exit 7.
	emitAndRun(t, "fn main() i32 { var tombstone_index ?uint = some 7; var result i32 = 0; if tombstone_index.has_value { let t = tombstone_index!; result = t as i32; } else { result = 3; } return result; }", false, 7, false)
}

func TestEmitOptionalPointerSomeHasValueCompilesAndRuns(t *testing.T) {
	// The exact minimal repro for pointer-payload optionals: a helper returns
	// `?*int` built from `some &y` / `none`, consumed in the entry via a
	// direct-call initializer and a `.has_value` read. The optional's .value
	// field is the pointee's pointer C type (int32_t *), the AddressOf child
	// flows through buildExpr's pointer path, and the has_value tag round-trips
	// through the call-returned optional struct. Only the tag is observed (the
	// pointed-to local is dead after the helper returns, so dereferencing
	// would be garbage — a language-semantics property, not a backend bug), so
	// exit 1 proves the some arm ran.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = 5; let r = find(v); if r.has_value { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitOptionalPointerForceUnwrapCompilesAndRuns(t *testing.T) {
	// The force-unwrap path for a pointer payload: `find(&v)!` unwraps an
	// optional-pointer call result (hoisted into a pebble_temp_<id> optional
	// local so the call runs exactly once) through the new
	// pebble_rt_checked_unwrap_ptr runtime helper, and the resulting pointer is
	// dereferenced. The pointer names a live local in the entry's frame, so
	// `*p` reads 5, not garbage (unlike the dangling &y shape).
	emitAndRun(t, "fn find(p *int) ?*int { return some p; } fn main() int { var v int = 5; let p = find(&v)!; return *p; }", false, 5, false)
}

func TestEmitOptionalPointerNoneHasValueCompilesAndRuns(t *testing.T) {
	// The none side of a pointer-payload optional: get_by_ref-style `return
	// none;` produces a { .has_value = false, .value = 0 } optional (a null
	// pointer constant 0 is a warning-clean initializer for the int32_t *
	// .value field), and the has_value read drives an if to the else arm.
	// Exit 0 proves the none path — not the some path — was taken.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = -1; let r = find(v); if r.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalPointerNoneLocalDeclCompilesAndRuns(t *testing.T) {
	// zeroOptionalPayloadLiteral's pointer-payload shape, in the one position
	// that exercises it as a local declaration's initializer: `var o ?*int =
	// none;`. The .value field's zero literal must be warning-clean against
	// the int32_t * field type (a bare 0, the null pointer constant — no
	// -Wmissing-braces shape needed for a scalar pointer). Exit 0 proves the
	// none tag.
	emitAndRun(t, "fn main() int { var o ?*int = none; if o.has_value { return 1; } else { return 0; } }", false, 0, false)
}

func TestEmitOptionalPointerSomeLocalDeclAndDerefCompilesAndRuns(t *testing.T) {
	// A some-initialized pointer-payload optional local (`var o ?*int = some
	// &y;`), unwrapped and dereferenced: `*(o!)` exercises the pointer-typed
	// CheckedOptionalUnwrap (via pebble_rt_checked_unwrap_ptr) feeding the
	// null-checked dereference, including the SourceAlias (grouped-expression
	// parens) the deref path introduces. y is alive in the entry frame, so the
	// deref reads 7. Exit 7.
	emitAndRun(t, "fn main() int { var y int = 7; var o ?*int = some &y; if !o.has_value { return 99; } return *(o!); }", false, 7, false)
}

func TestEmitOptionalPointerNoneForceUnwrapPanics(t *testing.T) {
	// A force-unwrap of an absent pointer-payload optional must panic with
	// PEBBLE_PANIC_UNWRAP_FAILED, exactly like every other payload width: the
	// pebble_rt_checked_unwrap_ptr helper has no null special-case — a null
	// payload VALUE is a valid unwrap result; only has_value=false faults.
	// find(-1) returns none, so the unwrap panics and the process terminates
	// abnormally.
	emitAndRun(t, "fn find(x int) ?*int { var y int = x; if x > 0 { return some &y; } return none; } fn main() int { var v int = -1; let p = find(v)!; return 1; }", false, 0, true)
}

func TestEmitOptionalPointerHmapGetByRefGetShapeCompilesAndRuns(t *testing.T) {
	// The real motivating fixture, mirroring std/hmap.peb's actual
	// get_by_ref/get shape WITHOUT touching the std module: get_by_ref returns
	// `?*int` built from `some &entry.value` (the address of a struct field
	// through a struct-pointer parameter) / `none`, and get consumes it via
	// `let ptr = get_by_ref(...)`, a `!ptr.has_value` guard, and `return some
	// *(ptr!);` — the exact statement sequence hmap.get performs. The exit
	// code 5 requires the whole chain to round-trip: get_by_ref's optional
	// pointer → get's has_value check → force-unwrap → dereference → some int.
	emitAndRun(t, "type Entry = struct { value int; }; fn get_by_ref(entry *Entry, key int) ?*int { if key < 0 { return none; } return some &entry.value; } fn get(entry *Entry, key int) ?int { let ptr = get_by_ref(entry, key); if !ptr.has_value { return none; } return some *(ptr!); } fn main() int { var e Entry = Entry.{ value = 5 }; let r = get(&e, 1); if r.has_value { return r!; } else { return 0; } }", false, 5, false)
}

func TestEmitOptionalPointerHmapGetByRefGetShapeNoneCompilesAndRuns(t *testing.T) {
	// The none side of the same hmap-shaped fixture: get_by_ref returns none
	// for a negative key, get sees !ptr.has_value and returns `none` (an
	// int-payload optional), and main's has_value check falls to the else arm.
	// Exit 0 proves both none paths (the ?*int and the ?int) round-trip.
	emitAndRun(t, "type Entry = struct { value int; }; fn get_by_ref(entry *Entry, key int) ?*int { if key < 0 { return none; } return some &entry.value; } fn get(entry *Entry, key int) ?int { let ptr = get_by_ref(entry, key); if !ptr.has_value { return none; } return some *(ptr!); } fn main() int { var e Entry = Entry.{ value = 5 }; let r = get(&e, -1); if r.has_value { return 99; } else { return 0; } }", false, 0, false)
}

func TestEmitStructTwoFieldReadBackCompilesAndRuns(t *testing.T) {
	// The confirmation fixture for struct construction and a field read: a
	// two-field Point { x i32; y i32; } is declared from a struct literal and
	// its x field is read back into the return value. The struct type emits
	// one struct typedef pebble_struct_<typeID>_t with one field per declared
	// struct field (named from each field's symbol ID), the local is
	// initialized with a designated-initializer struct literal, and the field
	// read lowers to pebble_local_<id>.pebble_field_<x>, so the process exit
	// code is x's value (1).
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ x = 1, y = 2 }; return point.x; }", false, 1, false)
}

func TestEmitStructFieldsWrittenOutOfDeclaredOrderCompilesAndRuns(t *testing.T) {
	// The out-of-declaration-order construction fixture: Point.{ y = ..., x =
	// ... } writes the fields in the opposite order from the struct's declared
	// order, so the RecordConstruct's Fields list is [y, x], not [x, y]
	// (confirmed against a real fixture dump). The designated-initializer
	// struct literal places each value under the C field its member symbol
	// names, so x still reads 22 and y still reads 20 regardless of the
	// construction order — proving the designated-initializer approach solves
	// the ordering problem rather than accidentally working because the test
	// happens to write fields in order.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"x read back", "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 20, x = 22 }; return point.x; }", 22},
		{"y read back", "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 20, x = 22 }; return point.y; }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStructBoolFieldDrivesIfCompilesAndRuns(t *testing.T) {
	// A struct with a bool field beside an integer field: the bool field read
	// (p.b) drives an if condition. b = true runs the then-arm (exit 10);
	// with the bool field false the else-arm runs (exit 20). The read is a
	// bool value, so it must route through the bool grammar (buildBoolExpr's
	// Load case), not the integer one.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = true }; if p.b { return 10; } else { return 20; } }", 10},
		{"bool false", "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = false }; if p.b { return 10; } else { return 20; } }", 20},
	} {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitStructFieldAsCallArgumentCompilesAndRuns(t *testing.T) {
	// A struct field read composes with 10.18's call-argument building: each
	// argument of add is a read of the struct local's field. buildCallArguments
	// builds each argument with buildExpr, which lowers the Load(FieldPlace)
	// read to pebble_local_<id>.pebble_field_<member>, so add(20, 22) = 42 is
	// the process exit code. The struct typedef must still be emitted before
	// the helper's definition.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return add(p.x, p.y); }", false, 42, false)
}

func TestEmitStructThreeFieldTwoReadsAddedCompilesAndRuns(t *testing.T) {
	// The "two fields read and added" fixture: a three-field struct's a and c
	// fields are read back and added: t.a + t.c = 10 + 30 = 40. The typedef's
	// fields follow the declared order (a, b, c), and the two reads resolve
	// their own fields by member symbol.
	emitAndRun(t, "type T = struct { a i32; b i32; c i32; };\nfn main() i32 { let t T = T.{ a = 10, b = 20, c = 30 }; return t.a + t.c; }", false, 40, false)
}

func TestEmitI64StructCompilesAndRuns(t *testing.T) {
	// The width discipline extends to struct field types: an i64 entry's
	// (i64, i64) struct's typedef fields are int64_t, and the field read feeds
	// the i64 entry's return. Exit code 22.
	emitAndRun(t, "type T = struct { a i64; b i64; };\nfn main() i64 { let t T = T.{ a = 20, b = 22 }; return t.b; }", false, 22, false)
}

func TestEmitStructLocalInsideHelperCompilesAndRuns(t *testing.T) {
	// A struct-typed local declared inside a reachable helper's body, not the
	// entry's: the typedef-collection pass must walk helper bodies too, so the
	// struct typedef is emitted before the helper's definition (which is
	// before pebble_user_main). helper declares point, reads its y field, and
	// returns it; the entry just calls helper, so exit code 22 proves the
	// helper's struct local was built and the typedef emitted correctly.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn helper() i32 { let point Point = Point.{ x = 20, y = 22 }; return point.y; } fn main() i32 { return helper(); }", false, 22, false)
}

func TestEmitStructContainingTupleCompilesAndRuns(t *testing.T) {
	src := "type HasTuple = struct { t (i32, i32); x i32; }; fn main() i32 { let t (i32, i32) = (20, 22); let h HasTuple = HasTuple.{ t = t, x = 1 }; return h.t.1; }"
	emitAndRun(t, src, false, 22, false)
}

func TestEmitTupleContainingStructCompilesAndRuns(t *testing.T) {
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let t (Point, i32) = (p, 1); return t.0.y; }"
	emitAndRun(t, src, false, 22, false)
}

func TestEmitArrayOfStructsCompilesAndRuns(t *testing.T) {
	src := "type Point = struct { x i32; y i32; }; fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; let a [2]Point = [p, p]; return a[1].x + a[1].y; }"
	emitAndRun(t, src, false, 42, false)
}

func TestEmitStructContainingOptionalCompilesAndRuns(t *testing.T) {
	src := "type HasOpt = struct { o ?i32; x i32; }; fn main() i32 { let o ?i32 = some 42; let h HasOpt = HasOpt.{ o = o, x = 1 }; return h.o!; }"
	emitAndRun(t, src, false, 42, false)
}

func TestEmitStructOutOfOrderWritesC(t *testing.T) {
	// The emitted C for the out-of-declaration-order construction fixture.
	// The typedef's fields must be in the struct's *declared* order (x = 25
	// then y = 26, from TypeDecl.Members), NOT the construction order the
	// RecordConstruct's Fields carry ([y, x] — confirmed against a real
	// fixture dump). The local initializer is a C99 designated-initializer
	// brace list placing each value under its own member's C field, and the
	// field read lowers to pebble_local_<id>.pebble_field_<member>. Symbols
	// 24 (Point), 25 (x), 26 (y), 28 (point), and struct type 23 come from the
	// real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { let point Point = Point.{ y = 2, x = 1 }; return point.x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_26 = 2, .pebble_field_25 = 1 };",
		"    (void)pebble_local_30;",
		"return pebble_local_30.pebble_field_25;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	mainIndex := strings.Index(out, "static int pebble_user_main")
	if typedefIndex < 0 || mainIndex < 0 || typedefIndex > mainIndex {
		t.Errorf("struct typedef does not precede pebble_user_main (definition before use):\n%s", out)
	}
}

func TestEmitStructBoolFieldWritesC(t *testing.T) {
	// The emitted C for the bool-field-if fixture: the typedef's second field
	// must be the C bool, the local's initializer carries the integer and bool
	// field values under their designated fields, and the if condition is the
	// raw field read pebble_local_<id>.pebble_field_<b> (a C bool needs no
	// comparison). Symbols 25 (a), 26 (b), 28 (p), and struct type 23 come
	// from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Pair = struct { a i32; b bool; };\nfn main() i32 { let p Pair = Pair.{ a = 1, b = true }; if p.b { return 10; } else { return 20; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    bool pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_25 = 1, .pebble_field_26 = true };",
		"    if (pebble_local_30.pebble_field_26) {\n",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStructFieldAsCallArgumentWritesC(t *testing.T) {
	// The emitted C for the call-argument fixture: the struct typedef precedes
	// both the helper and the entry, the local initializes to a designated
	// struct literal, and the call site passes the two field reads after ctx.
	// Symbols 24 (Point), 25/26 (x/y), 27 (add), 28/29 (its parameters), and
	// 31 (p) come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn add(a i32, b i32) i32 { return a + b; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return add(p.x, p.y); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_33 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_fn_27(ctx, pebble_local_33.pebble_field_25, pebble_local_33.pebble_field_26);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitI64StructWritesC(t *testing.T) {
	// The emitted C for the i64 struct must use int64_t for both typedef
	// fields and for the pebble_user_main return type, proving the entry's
	// width threads into the struct layout, not just the scalar declarations.
	unit, snapshot, entryID, sources := buildFixture(t, "type T = struct { a i64; b i64; };\nfn main() i64 { let t T = T.{ a = 20, b = 22 }; return t.b; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int64_t pebble_field_25;\n    int64_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_local_30.pebble_field_26;",
		"static int64_t pebble_user_main(PebbleContext *ctx)",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if strings.Contains(out, "int32_t pebble_field_") {
		t.Errorf("emitted C declared an i32 struct field for an i64 entry:\n%s", out)
	}
}

func TestEmitStrStructFieldLiteralC(t *testing.T) {
	// A struct whose field type is str now emits: the struct typedef declares
	// the field as the runtime's PebbleStr (the same C type a str local is
	// declared with), and a literal construction value lowers through
	// buildStrOperand's StringLiteral path to the same byte-oriented
	// `(PebbleStr){ .data = ..., .len = <N> }` compound literal a str local's
	// declaration embeds. Field order is preserved by the designated
	// initializer.
	unit, snapshot, entryID, sources := buildFixture(t, "type S = struct { s str; n i32; };\nfn main() i32 { let x S = S.{ n = 7, s = \"hi\" }; return x.n; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    PebbleStr pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_26 = 7, .pebble_field_25 = (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 } };",
		"return pebble_local_30.pebble_field_26;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitStrStructFieldLiteralRuns(t *testing.T) {
	// A struct with a str field constructed from a string literal compiles
	// and runs: the byte-oriented PebbleStr field is part of the struct
	// layout, the designated initializer places the literal into it, and the
	// (numeric) field read returns through the runtime. Reading the str field
	// itself back is a separate backend gap (Load(FieldPlace) in
	// buildStrOperand), so the value is verified here by the emitted-C test.
	emitAndRun(t, "type S = struct { s str; n int; };\nfn main() int { let x S = S.{ s = \"hi\", n = 5 }; return x.n; }", false, 5, false)
}

func TestEmitStrStructFieldLocalValueRuns(t *testing.T) {
	// A str field constructed from an in-scope str local (the SymbolValue
	// shape buildStrOperand accepts for a str call argument) compiles and
	// runs: the construction copies the local's PebbleStr by value into the
	// field.
	emitAndRun(t, "type S = struct { s str; n int; };\nfn main() int { let k str = \"ho\"; let x S = S.{ s = k, n = 6 }; return x.n; }", false, 6, false)
}

func TestEmitStrStructFieldCallValueRuns(t *testing.T) {
	// A str field constructed from a call to a str-returning helper (the
	// DirectCall shape buildStrOperand accepts) compiles and runs.
	emitAndRun(t, "fn mk() str { return \"hi\"; }\ntype S = struct { s str; n int; };\nfn main() int { let x S = S.{ s = mk(), n = 7 }; return x.n; }", false, 7, false)
}

func TestEmitStrStructFieldReadEqualityCompilesAndRuns(t *testing.T) {
	// The str field READ path (the missing Load(FieldPlace) case in
	// buildStrOperand): a struct's str field is read back and used as a str
	// operand in an existing str operation. The equality comparison lowers
	// through buildComparisonOperand's str branch, whose operand is the Load
	// of the str field, now emitted as the plain PebbleStr projection
	// pebble_local_<sym>.pebble_field_<member>. The field holds "hi", so
	// x.s == "hi" is true and the process exits 7 (the else arm would exit 3).
	emitAndRun(t, "type S = struct { s str; n int; };\nfn main() int { let x S = S.{ s = \"hi\", n = 5 }; if x.s == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrStructFieldReadGenericKeyCompilesAndRuns(t *testing.T) {
	// The exact std/hmap.peb shape without touching the std module: a generic
	// Entry[K, V] struct specialized with a str key (Entry[str, uint].key),
	// read back through the Load(FieldPlace) path buildStrOperand now
	// supports. The specialized field resolves through declaredFieldType's
	// substitution path, and the equality proves the read carried the str
	// value (exit 7, else 3).
	emitAndRun(t, "type Entry[K, V] = struct { key K; value V; };\nfn main() int { let e Entry[str, int] = Entry[str, int].{ key = \"hi\", value = 5 }; if e.key == \"hi\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrStructFieldReadAsCallArgumentCompilesAndRuns(t *testing.T) {
	// The str field read as a call-site argument (the exact std/hmap.peb
	// insert shape, `self.eq_fn(entry.key, key)`): a str-taking helper's
	// argument is built by buildStrOperand, which now accepts the
	// Load(FieldPlace) of the str field. The helper returns a==b, so passing
	// the field value and an equal literal exits 7 (else 3).
	emitAndRun(t, "type S = struct { s str; n int; };\nfn eq(a str, b str) bool { return a == b; }\nfn main() int { let x S = S.{ s = \"hi\", n = 5 }; if eq(x.s, \"hi\") { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrStructFieldReadWritesC(t *testing.T) {
	// The emitted C for the str field read: the Load(FieldPlace) of the str
	// field must lower to the plain projection pebble_local_<sym>.pebble_field_
	// <member> — the field's PebbleStr C type is exactly the str value's C
	// type, so no cast or coercion wraps the read, and the equality feeds it
	// straight into pebble_rt_str_eq. Symbols 25 (s), 26 (n), 30 (x) come
	// from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type S = struct { s str; n int; };\nfn main() int { let x S = S.{ s = \"hi\", n = 5 }; if x.s == \"hi\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "if (pebble_rt_str_eq(pebble_local_30.pebble_field_25, (PebbleStr){ .data = (const uint8_t *)\"hi\", .len = 2 })) {") {
		t.Errorf("emitted C missing the str field-read projection fed to pebble_rt_str_eq:\n%s", out)
	}
}

func TestEmitStructStrFieldAssignmentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `type Holder = struct { value str; }; fn main() i32 { var h Holder = Holder.{ value = "old" }; var replacement str = "new"; h.value = replacement; if h.value == "new" { return 0; } return 1; }`, false, 0, false)
}

func TestEmitInlineTupleArgumentCompilesAndRuns(t *testing.T) {
	// The flagship 10.25 tuple fixture (10.24's TestEmitRejectsTupleParameter
	// fixture, now the positive case): a freshly-constructed tuple built inline
	// at the call site — f((20, 22)) — rather than passed as an already-
	// declared local. The DirectCall argument is a TupleValue carrying the
	// same Children/Type shape it has as a local's declaration initializer
	// (confirmed against a real fixture dump), built by buildTupleValueExpr as
	// the positional C99 compound literal (pebble_tuple_<typeID>_t){ 20, 22 }.
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f((20, 22)); }", false, 42, false)
}

func TestEmitInlineStructArgumentCompilesAndRuns(t *testing.T) {
	// The flagship 10.25 struct fixture (10.24's
	// TestEmitRejectsStructTypedParameter fixture, now the positive case): a
	// freshly-constructed struct built inline at the call site —
	// f(Point.{ x = 20, y = 22 }) — rather than passed as an already-declared
	// local. The DirectCall argument is a RecordConstruct carrying the same
	// Fields/Type shape it has as a local's declaration initializer (confirmed
	// against a real fixture dump), built by buildStructValueExpr as the
	// designated-initializer C99 compound literal
	// (pebble_struct_<typeID>_t){ .pebble_field_<x> = 20,
	// .pebble_field_<y> = 22 }. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ x = 20, y = 22 }); }", false, 42, false)
}

func TestEmitInlineStructArgumentOutOfOrderCompilesAndRuns(t *testing.T) {
	// The designated-initializer reuse, verified in the inline-argument
	// position too: Point.{ y = 22, x = 20 } writes the fields in the opposite
	// order from the struct's declared order, and the compound literal still
	// places each value under the C field its member symbol names, so x reads
	// 20 and y reads 22 and the sum is 42. This is the argument-position analog
	// of TestEmitStructFieldsWrittenOutOfDeclaredOrderCompilesAndRuns — the
	// reordering problem is solved by the designated-initializer form in the
	// inline position exactly as it is in a local declaration, not just by a
	// fixture that happens to write fields in order.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(Point.{ y = 22, x = 20 }); }", false, 42, false)
}

func TestEmitStructParameterCompilesAndRuns(t *testing.T) {
	// The flagship struct-parameter fixture: f takes a whole Point struct as
	// its parameter and reads both fields back inside the callee. The entry
	// declares a struct local and passes it by value; the callee's parameter
	// seeds its own scope as a struct local, so the reads resolve through the
	// same Load(FieldPlace) machinery a struct local uses. 20 + 22 = 42 is the
	// process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return f(p); }", false, 42, false)
}

func TestEmitPointerStructFieldReadWriteCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "type P = struct { cap i32; }; fn mutate(p *P) void { p.cap = 9; } fn main() i32 { var p P = P.{ cap = 1 }; let pointer *P = &p; mutate(pointer); return p.cap; }", false, 9, false)
}

func TestEmitStructParameterWritesC(t *testing.T) {
	// The emitted C for the flagship fixture: the struct typedef must precede
	// the helper, the helper's signature declares the parameter as
	// pebble_struct_<typeID>_t pebble_local_<paramSymbol> (symbol 28, the p
	// parameter), the parameter gets the same (void) cast, the body reads
	// pebble_local_28.pebble_field_25 / .pebble_field_26, and the call site
	// passes the entry's struct local pebble_local_30 directly (no construction
	// at the call site). Symbols 24 (Point), 25 (x), 26 (y), 27 (f), 28 (p
	// param), 29 (main), 30 (p local), and struct type 23 come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x + p.y; } fn main() i32 { let p Point = Point.{ x = 20, y = 22 }; return f(p); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"static int32_t pebble_fn_27(PebbleContext *ctx, pebble_struct_23_t pebble_local_28) {",
		"    (void)pebble_local_28;",
		"    return pebble_rt_checked_add_i32(pebble_local_28.pebble_field_25, pebble_local_28.pebble_field_26, (PebbleSourceLoc){\"main.peb\", 2, 28});",
		"pebble_struct_23_t pebble_local_32 = { .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"return pebble_fn_27(ctx, pebble_local_32);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static int32_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitStructParameterBoolFieldDrivesIfCompilesAndRuns(t *testing.T) {
	// A struct parameter whose fields are mixed width/bool: the callee reads
	// the bool field to drive an if and an integer field as a value. This
	// proves the parameter's field reads route through buildBoolExpr (the
	// Load(FieldPlace) bool path) exactly as a struct local's do. With b true
	// the then-arm runs and returns the x field 10.
	emitAndRun(t, "type Pair = struct { x i32; b bool; };\nfn f(p Pair) i32 { if p.b { return p.x; } else { return 99; } } fn main() i32 { let p Pair = Pair.{ x = 10, b = true }; return f(p); }", false, 10, false)
}

func TestEmitTupleReturningHelperCompilesAndRuns(t *testing.T) {
	// The flagship tuple-return fixture: makeT returns a fresh (i32, i32)
	// tuple constructed inline in its return statement, the entry declares a
	// matching tuple local from the call (the one supported call position for
	// a tuple-returning helper — the direct initializer of a matching local),
	// and reads both elements back. The helper's C signature must declare its
	// return type as the tuple's own typedef, its return statement must emit
	// the compound-literal expression, and the call site must initialize the
	// local from the call. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", false, 42, false)
}

func TestEmitStructReturningHelperCompilesAndRuns(t *testing.T) {
	// The flagship struct-return fixture, mirroring the tuple one: makeP
	// returns a fresh Point constructed inline in its return statement, the
	// entry declares a matching struct local from the call and reads both
	// fields back. The designated-initializer compound-literal return value is
	// exercised end-to-end. 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", false, 42, false)
}

func TestEmitTupleReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper whose return statement forwards an
	// already-declared aggregate-typed local (a plain SymbolValue, not a fresh
	// construction): x is declared in the helper's body from a tuple literal,
	// and `return x;` forwards it, emitting `return pebble_local_<x>;`. The
	// entry assigns the call to a matching local and reads both elements back.
	// 20 + 22 = 42 is the process exit code.
	emitAndRun(t, "fn makeT() (i32, i32) { let x (i32, i32) = (20, 22); return x; } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", false, 42, false)
}

func TestEmitStructReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// The struct side of forwarding an already-declared local: p is declared
	// in the helper's body from a struct literal and `return p;` forwards it,
	// emitting `return pebble_local_<p>;`. The entry assigns the call to a
	// matching local and reads both fields back. 20 + 22 = 42 is the process
	// exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { let p Point = Point.{ x = 20, y = 22 }; return p; } fn main() i32 { let q Point = makeP(); return q.x + q.y; }", false, 42, false)
}

func TestEmitStructReturningHelperForwardsCallCompilesAndRuns(t *testing.T) {
	// A struct-returning helper whose tail return is a DirectCall to another
	// struct-returning helper (`return helper();` — the return-forwarding
	// shape io.peb's `return string::new();` uses), with no intermediate
	// local: makeP's return value is built from the call itself rather than
	// from a SymbolValue or RecordConstruct. The entry assigns makeP's call
	// to a matching local and reads both fields back, so a real field of the
	// returned struct must come through end-to-end. 20 + 22 = 42 is the
	// process exit code.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn helper() Point { return Point.{ x = 20, y = 22 }; } fn makeP() Point { return helper(); } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", false, 42, false)
}

func TestEmitTupleReturningHelperWithBoolElementCompilesAndRuns(t *testing.T) {
	// A mixed width/bool tuple result: the bool element is built by
	// buildBoolExpr inside the tuple's brace list (proving the element grammar
	// dispatch in the return path, not just the local-declaration path), and
	// once read back in the entry it drives an if. With t.1 true the then-arm
	// returns the i32 element 20.
	emitAndRun(t, "fn makeT() (i32, bool) { return (20, true); } fn main() i32 { let t (i32, bool) = makeT(); if t.1 { return t.0; } else { return 99; } }", false, 20, false)
}

func TestEmitTupleReturningHelperIfElseTailCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper whose body tail is a two-armed if/else (not a
	// bare return): each arm's return is a fresh tuple construction, proving
	// buildIf threads the enclosing function's resultInfo into both arms so
	// each arm's Return routes through buildAggregateReturnValue. With the flag
	// true the then-arm's (20, 22) wins, and 20 + 22 = 42 is the exit code.
	emitAndRun(t, "fn pick(b bool) (i32, i32) { if b { return (20, 22); } else { return (0, 0); } } fn main() i32 { let t (i32, i32) = pick(true); return t.0 + t.1; }", false, 42, false)
}

func TestEmitTupleReturningHelperWritesC(t *testing.T) {
	// The emitted C for the tuple flagship: the tuple typedef must precede the
	// helper, the helper's signature declares its return type as
	// pebble_tuple_23_t (the aggregate's own typedef, not the entry's scalar
	// int32_t), its return statement emits the C99 compound-literal expression
	// (pebble_tuple_23_t){ 20, 22 }, and the call site initializes the local
	// directly from pebble_fn_24(ctx). Symbols 24 (makeT), 25 (main), 26 (t
	// local), and tuple type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"static pebble_tuple_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_tuple_23_t){ 20, 22 };",
		"pebble_tuple_23_t pebble_local_28 = pebble_fn_24(ctx);",
		"    (void)pebble_local_28;",
		"return pebble_rt_checked_add_i32(pebble_local_28._0, pebble_local_28._1, (PebbleSourceLoc){\"main.peb\", 1, 95});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_tuple_23_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("tuple typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitStructReturningHelperWritesC(t *testing.T) {
	// The emitted C for the struct flagship: the struct typedef precedes the
	// helper, the helper's signature declares its return type as
	// pebble_struct_23_t, its return statement emits the designated-
	// initializer compound-literal expression, and the call site initializes
	// the local from pebble_fn_27(ctx). Symbols 24 (Point), 25 (x), 26 (y), 27
	// (makeP), 28 (main), 29 (p local), and struct type 23 come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return p.x + p.y; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t pebble_field_25;\n    int32_t pebble_field_26;\n} pebble_struct_23_t;",
		"static pebble_struct_23_t pebble_fn_27(PebbleContext *ctx) {",
		"    return (pebble_struct_23_t){ .pebble_field_25 = 20, .pebble_field_26 = 22 };",
		"pebble_struct_23_t pebble_local_31 = pebble_fn_27(ctx);",
		"return pebble_rt_checked_add_i32(pebble_local_31.pebble_field_25, pebble_local_31.pebble_field_26, (PebbleSourceLoc){\"main.peb\", 2, 101});",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_struct_23_t pebble_fn_27")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("struct typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitTupleReturningHelperForwardsLocalWritesC(t *testing.T) {
	// The emitted C for the local-forwarding return: the helper's return
	// statement emits `return pebble_local_<x>;` (the already-declared local's
	// own C name, no re-construction). Symbols 24 (makeT), 25 (main), 26 (x
	// local), 27 (t local), and tuple type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn makeT() (i32, i32) { let x (i32, i32) = (20, 22); return x; } fn main() i32 { let t (i32, i32) = makeT(); return t.0 + t.1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static pebble_tuple_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    pebble_tuple_23_t pebble_local_28 = { 20, 22 };",
		"    return pebble_local_28;",
		"pebble_tuple_23_t pebble_local_29 = pebble_fn_24(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitTupleReturnResultTypeOnlyHelperCompilesAndRuns(t *testing.T) {
	// The typedef-discovery fixture (10.26): the (i32, i32) tuple type appears
	// only as makeT's result type — makeT constructs a tuple of that type in
	// its return (never anywhere in main, which only assigns the call to a
	// matching local and never reads an element or constructs a tuple), so the
	// emitted helper's C signature still names pebble_tuple_<typeID>_t and the
	// typedef must be emitted before it. The program compiles clean under the
	// strict flags and exits 0.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let t (i32, i32) = makeT(); return 0; }", false, 0, false)
}

func TestEmitStructReturnResultTypeOnlyHelperCompilesAndRuns(t *testing.T) {
	// The struct side of the typedef-discovery fixture: the Point type appears
	// only as makeP's result type — makeP constructs a Point in its return,
	// and main only assigns the call to a matching local and never reads a
	// field or constructs a Point — yet the typedef must still be emitted
	// before the helper whose C signature names pebble_struct_<typeID>_t. The
	// program compiles clean under the strict flags and exits 0.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn main() i32 { let p Point = makeP(); return 0; }", false, 0, false)
}

func TestEmitTupleReturningHelperInIfArmLocalInitializerCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper called from an if/else arm's own local
	// declaration: the arm is a block built by the same recursive buildBlock,
	// so the DirectCall initializer is handled by the identical
	// buildTupleLocalDeclaration path the top-level case uses — no special
	// plumbing for the nested position. With the flag true the then-arm
	// declares t from makeT() and returns t.0 = 20.
	emitAndRun(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { let b bool = true; if b { let t (i32, i32) = makeT(); return t.0; } else { return 0; } }", false, 20, false)
}

func TestEmitTupleReturningHelperInLoopBodyLocalInitializerCompilesAndRuns(t *testing.T) {
	// A tuple-returning helper called from a while loop body's local
	// declaration: the loop body is built by buildLoopBody, whose leading
	// statements go through the same buildLeadingStatement /
	// buildTupleLocalDeclaration path, so the DirectCall initializer works in
	// the nested position without special-casing. The loop declares t from
	// makeT() once, accumulates both elements, and the entry returns the sum
	// 42. Bounded execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { var n i32 = 0; var s i32 = 0; while n < 1 { let t (i32, i32) = makeT(); s = s + t.0 + t.1; n = n + 1; } return s; }", false, 42, false)
}

func TestEmitEnumSwitchLeadingCompilesAndRuns(t *testing.T) {
	// An enum-typed subject in a fall-through (leading-position) switch: the
	// subject-building and CaseValue label logic shared with the tail-position
	// buildSwitch is reused unchanged by buildLoopSwitch. c = green hits the
	// CaseValue-based case (total = 10), then the fall-through code adds 1,
	// so total = 11.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar total i32 = 0;\nvar c Color = Color.green;\nswitch c { case Color.red: total = total + 1; case Color.green: total = total + 10; case Color.blue: total = total + 100; }\ntotal = total + 1;\nreturn total;\n}", false, 11, false)
}

func TestEmitEnumLocalSwitchGreenCompilesAndRuns(t *testing.T) {
	// The flagship fixture from the brief: a plain enum local declared from a
	// variant literal and switched on, each case returning a distinct value.
	// c is Color.green (variant 26, ordinal 1), so the green case fires and the
	// exit code is 1. The emitted C typedef assigns pebble_variant_26 the value
	// 1 by declaration order, and the C switch compares the local's stored
	// constant against the case labels, so the right case fires end to end.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 1, false)
}

func TestEmitEnumLocalSwitchBlueCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.blue (variant 27, ordinal 2): the blue case fires
	// and the exit code is 2, proving a second variant value dispatches to a
	// different C case label rather than the switch only ever firing the first
	// case.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 2, false)
}

func TestEmitEnumLocalSwitchRedCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.red (variant 25, ordinal 0): the red case fires
	// and the exit code is 0, proving the first declared variant (ordinal 0)
	// matches case label pebble_variant_25 — the 0 constant C assigns first.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 0, false)
}

func TestEmitEnumSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// A multi-value case on an enum subject: `case Color.red, Color.green:`
	// produces two SwitchCase nodes sharing one body node ID (confirmed against
	// a real fixture), which must stack as two C case labels sharing one body,
	// exactly as 10.31's integer multi-value cases do. c = Color.green hits the
	// multi-value case and returns 10.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red, Color.green: return 10; case Color.blue: return 20; }\n}", false, 10, false)
}

func TestEmitEnumSwitchMultiValueCaseOtherVariantCompilesAndRuns(t *testing.T) {
	// Same multi-value switch with c = Color.red: the other member of the
	// multi-value case fires, proving both stacked case labels share the body.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red, Color.green: return 10; case Color.blue: return 20; }\n}", false, 10, false)
}

func TestEmitEnumSwitchElseCompilesAndRuns(t *testing.T) {
	// An else arm on an enum switch: c = Color.blue is covered by no case
	// (only red and green have cases), so the else/default arm fires and
	// returns 20.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red, Color.green: return 10; else: return 20; }\n}", false, 20, false)
}

func TestEmitEnumSwitchElseCaseHitCompilesAndRuns(t *testing.T) {
	// Same switch, c = Color.green: the case fires (10), not the else arm,
	// proving the else/default arm is not selected when a case matches.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red, Color.green: return 10; else: return 20; }\n}", false, 10, false)
}

func TestEmitEnumBlockCaseBodyCompilesAndRuns(t *testing.T) {
	// A block-wrapped (braced, multi-statement) case body on an enum switch,
	// exercising the Block-bodied path in buildSwitchCaseBody for an enum
	// subject: the case declares a local and returns an expression using it.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nswitch c { case Color.red: { let x i32 = 42; return x; } case Color.green: return 1; case Color.blue: return 2; }\n}", false, 42, false)
}

func TestEmitEnumSwitchBareReturnCaseBodyCompilesAndRuns(t *testing.T) {
	// A bare single-statement case body (no braces) on an enum switch,
	// exercising the bare-Return path in buildSwitchCaseBody.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.blue;\nswitch c { case Color.red: return 1; case Color.green: return 2; case Color.blue: return 3; }\n}", false, 3, false)
}

func TestEmitEnumSwitchBaselessCaseLabelsCompileAndRun(t *testing.T) {
	// The base-less .name shorthand for switch case labels (the exact repro:
	// `case .red:` against a Color-typed subject). Each variant's subject must
	// fire its own case branch and exit with the matching code, proving the
	// shorthand resolves to the same member as the qualified `case Color.red:`
	// form rather than merely checking clean. No trailing return after the
	// switch: the shorthand must be recognized as exhaustive by
	// switchIsExhaustive (control_validation.go) exactly like a qualified
	// label already is, not merely resolve without also covering
	// exhaustiveness — a masked version of this test with a spurious trailing
	// return previously hid a real gap there (base-less case values produce
	// an aggregateEnumVariant record, not a memberVariant member record, so
	// switchIsExhaustive's coverage set never saw them and every base-less
	// switch spuriously required an unreachable trailing return).
	for _, fixture := range []struct {
		name  string
		value string
		want  int
	}{
		{"red", "Color.red", 1},
		{"green", "Color.green", 2},
		{"blue", "Color.blue", 3},
	} {
		t.Run(fixture.name, func(t *testing.T) {
			emitAndRun(t, "type Color = enum { red, green, blue }; fn main() int {\nvar c Color = "+fixture.value+";\nswitch c { case .red: return 1; case .green: return 2; case .blue: return 3; }\n}", false, fixture.want, false)
		})
	}
}

func TestEmitTaggedUnionSwitchBaselessCaseLabelsCompileAndRun(t *testing.T) {
	// Same base-less .name shorthand as
	// TestEmitEnumSwitchBaselessCaseLabelsCompileAndRun, against a tagged
	// union (union enum) subject instead of a plain enum — confirms the
	// exhaustiveness fix (switchIsExhaustive now also indexes
	// aggregateTaggedVariant records) covers both nominal-declaration kinds,
	// not just plain enums. No trailing return: the switch must be
	// recognized as exhaustive.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case .empty: return 0; case .value: return 1; }\n}", false, 1, false)
}

func TestEmitEnumStoreCompilesAndRuns(t *testing.T) {
	// Reassigning an enum-typed local (c = Color.red; after declaration) lowers
	// through buildStoreCore's enum branch to
	// `pebble_local_<sym> = pebble_variant_<red>;`, so the subsequent switch
	// fires the red case. This proves the value actually changed, not just that
	// the store compiled.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nc = Color.red;\nswitch c { case Color.red: return 7; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 7, false)
}

func TestEmitEnumVariantCallFormCompilesAndRuns(t *testing.T) {
	// A plain enum variant written with explicit empty parens — Color.red() —
	// is a zero-payload VariantConstruct (confirmed against a real fixture),
	// the same discriminant value as Color.red, so it is accepted as an enum
	// local's initializer and the red case fires.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red();\nswitch c { case Color.red: return 9; case Color.green: return 1; case Color.blue: return 2; }\n}", false, 9, false)
}

func TestEmitEnumEqualityCompilesAndRuns(t *testing.T) {
	// Equality between enum values, c == Color.red — confirmed checker-
	// reachable (it produces a BinaryValue with two enum-typed operands), so it
	// lowers through buildComparison's enum branch to the plain C == on the
	// two enum constants. With c = green the comparison is false and the else
	// arm returns 5.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nif c == Color.red { return 1; } else { return 5; }\n}", false, 5, false)
}

func TestEmitEnumEqualityTrueCompilesAndRuns(t *testing.T) {
	// The == comparison with c = red is true, proving the equality actually
	// evaluates rather than always falling to the else arm.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nif c == Color.red { return 1; } else { return 5; }\n}", false, 1, false)
}

func TestEmitEnumShorthandComparisonAndAssignmentCompilesAndRuns(t *testing.T) {
	// Enum-variant shorthand literals (.Empty/.Occupied) in a var initializer,
	// an equality comparison, and an assignment. The loop body is the one
	// statement position where the backend already supports mid-body conditions
	// (the entry/helper body grammar allows only leading declarations, stores,
	// and calls followed by a tail return/if/switch), so the one-iteration
	// loop hosts the branching. The local starts .Empty: the first comparison
	// correctly does NOT fire, the local is reassigned .Occupied, and the
	// second comparison fires, accumulating n = 42. Every .Empty is a
	// deferred-receiver aggregate resolved from the solved enum type, and the
	// emitted C compares and assigns the enum's discriminant constants, so the
	// runtime branch behavior proves the shorthand lowered correctly.
	emitAndRunBounded(t, "type State = enum { Empty, Occupied };\nfn main() i32 {\nvar n i32 = 0;\nvar done i32 = 0;\nwhile done == 0 {\nvar s State = .Empty;\nif s == .Occupied { n = n + 1; }\ns = .Occupied;\nif s == .Occupied { n = n + 42; }\ndone = 1;\n}\nreturn n;\n}", false, 42, false)
}

func TestEmitEnumOrderingCompilesAndRuns(t *testing.T) {
	// An ordering comparison on enum values, c < Color.blue — also confirmed
	// checker-reachable (the checker accepts it, unlike bool ordering). Both
	// operands lower to their enum constants (green = ordinal 1, blue = ordinal
	// 2), and the plain C < on the discriminants is the direct, correct
	// lowering: 1 < 2 is true, so the then-arm returns 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nif c < Color.blue { return 1; } else { return 5; }\n}", false, 1, false)
}

func TestEmitEnumWhileConditionCompilesAndRuns(t *testing.T) {
	// An enum equality comparison as a while condition, reassigning the enum
	// local inside the loop so the loop terminates. The first iteration sees
	// c == Color.red true (c = red), so the loop runs once, reassigns c to
	// blue, and exits on the second condition check. Bounded execution because
	// the loop's own condition is the only bound.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nvar n i32 = 0;\nwhile c == Color.red { n = n + 1; c = Color.blue; }\nreturn n;\n}", false, 1, false)
}

func TestEmitEnumLocalUnusedCompilesClean(t *testing.T) {
	// A plain enum local declared and never referenced after its declaration
	// still compiles clean under -Wall -Wextra -Werror: the emitted declaration
	// is followed by the same (void) cast every other local gets, so the strict
	// build never warns about an unused variable.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nreturn 3;\n}", false, 3, false)
}

func TestEmitEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly for one fixture: the enum typedef's exact
	// shape (one named constant per variant, in declared order, named from each
	// variant's symbol ID; the typedef named from the enum's type ID), the
	// enum-typed local's declaration initializing from the variant's constant,
	// and the switch's case labels naming the same constants.
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}")
	if len(variants) != 3 {
		t.Fatalf("fixture has %d variants, want 3 (red, green, blue)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	typedef := "typedef enum {\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[0])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[1])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[2])) + ",\n" +
		"} " + enumTypeName(enumType) + ";"
	if !strings.Contains(out, typedef) {
		t.Errorf("emitted C is missing the enum typedef %q:\n%s", typedef, out)
	}
	for _, want := range []string{
		enumTypeName(enumType) + " pebble_local_",
		"= pebble_variant_" + strconv.Itoa(int(variants[1])) + ";",
		"switch (pebble_local_",
		"case pebble_variant_" + strconv.Itoa(int(variants[0])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[1])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[2])) + ":",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitEnumToIntegerCompilesAndRuns(t *testing.T) {
	// The exact minimal repro from the brief: `Color.green as i32` in an i32
	// entry. The checker lowers the cast to a tir.EnumToInteger whose single
	// child is the EnumVariantValue Color.green, and the backend lowers it to a
	// plain C cast (int32_t)(pebble_variant_<green>) — green's ordinal in
	// declared order is 1, so the exit code is 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.green as i32; }", false, 1, false)
}

func TestEmitEnumToIntegerZeroOrdinalCompilesAndRuns(t *testing.T) {
	// The first-declared variant (red) has C ordinal 0, so casting it to an
	// integer yields 0 — proving the cast reads the actual declared-order
	// discriminant rather than always producing a nonzero value.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.red as i32; }", false, 0, false)
}

func TestEmitEnumToIntegerI64CompilesAndRuns(t *testing.T) {
	// A different destination integer width than the entry's result type: the
	// cast's destination (i64) is the entry's own width here, so the
	// EnumToInteger node's Type matches the surrounding width gate exactly as an
	// IntegerCast's does. green's ordinal 1 is returned as an i64.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i64 { return Color.green as i64; }", false, 1, false)
}

func TestEmitEnumToIntegerUnsignedNestedCompilesAndRuns(t *testing.T) {
	// An unsigned destination (u32) inside an i32 entry: a cast whose
	// destination is not the entry's width is only valid where the surrounding
	// context is that width, so it appears as a u32 local's initializer, then
	// the local is read back out as i32. The destination width is resolved from
	// the EnumToInteger node's own Type (u32 -> uint32_t), not from the entry.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { let v u32 = Color.green as u32; return v as i32; }", false, 1, false)
}

func TestEmitEnumToIntegerFromLocalCompilesAndRuns(t *testing.T) {
	// An enum value read from a local (not just a variant literal) cast to an
	// integer: the EnumToInteger's child is a SymbolValue naming the enum-typed
	// local, built by buildEnumValue as pebble_local_<sym> and cast directly.
	// c = blue, whose ordinal is 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 { var c Color = Color.blue; return c as i32; }", false, 2, false)
}

func TestEmitEnumToIntegerFromLocalI64CompilesAndRuns(t *testing.T) {
	// The from-a-local form at a different width (i64): the local's value is
	// read and cast to the destination width, again via the same
	// buildEnumValue + plain C cast lowering.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i64 { var c Color = Color.blue; return c as i64; }", false, 2, false)
}

func TestEmitEnumToIntegerWritesC(t *testing.T) {
	// Confirm the emitted C directly: the EnumToInteger lowers to a plain C
	// cast of the enum constant to the destination C type, (int32_t)(...), with
	// no runtime helper and no intermediate enum-typedef step.
	unit, snapshot, entryID, enumType, variants, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 { return Color.green as i32; }")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	cast := "(int32_t)(pebble_variant_" + strconv.Itoa(int(variants[1])) + ")"
	if !strings.Contains(out, cast) {
		t.Errorf("emitted C missing the enum-to-integer cast %q:\n%s", cast, out)
	}
	for _, want := range []string{
		"typedef enum {",
		"} " + enumTypeName(enumType) + ";",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitCheckedIntegerToEnumOutOfRangeSafePanics(t *testing.T) {
	// The minimal repro: `5 as Color` where Color has three variants (red,
	// green, blue, ordinals 0-2) — 5 names no real variant. Compiled in
	// PEBBLE_RT_MODE_SAFE, the cast must panic through the runtime's checked
	// primitive before the enum value is ever used, so the process terminates
	// abnormally rather than exiting 0. The cast's destination enum type is
	// declared only as a local's type (never constructed or switched on), so
	// this also proves the enum typedef is still emitted for a cast-reached
	// enum.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 5 as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumOutOfRangeReleaseDoesNotPanic(t *testing.T) {
	// The same `5 as Color` in PEBBLE_RT_MODE_RELEASE: the bounds check is
	// skipped entirely (release trusts the input), so the cast produces some
	// enum value and the program runs to its return 0. Which enum value it is
	// is explicitly NOT asserted — release is unchecked, so any value is
	// acceptable; the assertion is only that the program did not crash.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 5 as Color;\nreturn 0;\n}", 0, false)
}

func TestEmitCheckedIntegerToEnumValidRoundTripsSafe(t *testing.T) {
	// An in-range cast, `1 as Color` (ordinal 1, green), verified end to end
	// by casting it back with EnumToInteger: the round-trip value must be 1 in
	// SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}", false, 1, false)
}

func TestEmitCheckedIntegerToEnumValidRoundTripsRelease(t *testing.T) {
	// The same in-range `1 as Color` round trip in RELEASE mode: the unchecked
	// cast passes 1 through, cast back to i32 it is still 1.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}", 1, false)
}

func TestEmitCheckedIntegerToEnumNegativeSafePanics(t *testing.T) {
	// `-1 as Color`: a genuinely negative signed source. The backend widens it
	// to int64_t (sign-extending), and the primitive's unsigned comparison
	// (uint64_t)(-1) >= (uint64_t)3 must reject it — the exact case the
	// unsigned-comparison design exists to get right. SAFE mode panics.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = -1 as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumNegativeReleaseDoesNotPanic(t *testing.T) {
	// The same `-1 as Color` in RELEASE mode: no bounds check, so the program
	// runs to its return 0 instead of panicking.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = -1 as Color;\nreturn 0;\n}", 0, false)
}

func TestEmitCheckedIntegerToEnumFromLocalSafePanics(t *testing.T) {
	// A source value from a local (not a literal): `n as Color` where n is an
	// i32 local holding 5 — out of range for a 3-variant enum. The local read
	// feeds the same checked cast; SAFE mode panics.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet n i32 = 5;\nlet c Color = n as Color;\nreturn 0;\n}", false, 0, true)
}

func TestEmitCheckedIntegerToEnumFromLocalValidRoundTripsSafe(t *testing.T) {
	// An in-range local source: n = 2, `n as Color` is ordinal 2 (blue),
	// round-tripped back through EnumToInteger to 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet n i32 = 2;\nlet c Color = n as Color;\nreturn c as i32;\n}", false, 2, false)
}

func TestEmitCheckedIntegerToEnumStorePositionCompilesAndRuns(t *testing.T) {
	// A store-position cast: `c = 2 as Color;` reassigns an already-declared
	// enum local (buildStoreCore's enum branch routes the value through
	// buildEnumValue, which now accepts the cast), so the store lands ordinal 2
	// (blue) and the round trip returns 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c Color = Color.red;\nc = 2 as Color;\nreturn c as i32;\n}", false, 2, false)
}

func TestEmitCheckedIntegerToEnumComparisonPositionCompilesAndRuns(t *testing.T) {
	// A comparison-position cast: `(2 as Color) == Color.blue` compares the
	// cast result against a variant literal (buildComparison's enum branch),
	// which is true since the cast produces ordinal 2.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nif (2 as Color) == Color.blue { return 1; } else { return 0; }\n}", false, 1, false)
}

func TestEmitCheckedIntegerToEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly: the cast lowers to
	// `(<enum typedef>)pebble_rt_checked_int_to_enum((int64_t)(<child>),
	// <variant_count>, <loc>)` — the source widened to the primitive's int64_t
	// input, the variant count from the destination enum's 3 declared members,
	// and the result narrowed back to the enum typedef.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nlet c Color = 1 as Color;\nreturn c as i32;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	want := "(" + enumTypeName(enumType) + ")pebble_rt_checked_int_to_enum((int64_t)(1), 3, (PebbleSourceLoc)"
	if !strings.Contains(out, want) {
		t.Errorf("emitted C missing the integer-to-enum cast call %q:\n%s", want, out)
	}
	for _, want := range []string{
		"typedef enum {",
		"} " + enumTypeName(enumType) + ";",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitOptionalIntegerToEnumOutOfRangeHasValueFalseSafe(t *testing.T) {
	// The exact repro: `var c ?Color = 5 as ?Color;` where Color has three
	// variants (red, green, blue, ordinals 0-2) — 5 names no real variant.
	// The cast must produce an optional whose has_value is false, verified by
	// reading c.has_value directly. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nif c.has_value { return 1; } else { return 0; }\n}", false, 0, false)
}

func TestEmitOptionalIntegerToEnumOutOfRangeHasValueFalseRelease(t *testing.T) {
	// The same `var c ?Color = 5 as ?Color;` in PEBBLE_RT_MODE_RELEASE.
	// Unlike the checked cast (which skips its check in RELEASE), the
	// optional's validity query must be correct in BOTH modes — a wrong
	// has_value would be silently incorrect, not merely unchecked — so
	// has_value is false here too, exactly as in SAFE.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nif c.has_value { return 1; } else { return 0; }\n}", 0, false)
}

func TestEmitOptionalIntegerToEnumOutOfRangeUnwrapPanicsSafe(t *testing.T) {
	// The strongest confirmation that has_value is false: force-unwrapping
	// the absent optional (`c!`) panics through
	// pebble_rt_checked_unwrap_i32 in SAFE mode, so the process terminates
	// abnormally rather than returning anything.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nreturn c! as i32;\n}", false, 0, true)
}

func TestEmitOptionalIntegerToEnumOutOfRangeUnwrapPanicsRelease(t *testing.T) {
	// The same force-unwrap in RELEASE: unwrapping an absent optional panics
	// in every configuration (the runtime's unwrap is not mode-gated), so
	// the process terminates abnormally here too — and the panicking unwrap
	// is itself the proof that has_value is false in RELEASE as well.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 5 as ?Color;\nreturn c! as i32;\n}", 0, true)
}

func TestEmitOptionalIntegerToEnumValidRoundTripsSafe(t *testing.T) {
	// An in-range cast, `1 as ?Color` (ordinal 1, green): has_value must be
	// true, and the unwrapped value must be the green variant — verified by a
	// round trip through EnumToInteger, exactly the CheckedIntegerToEnum
	// pattern (`return c as i32` == 1), with the unwrap reading the optional
	// local's stored enum value. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", false, 1, false)
}

func TestEmitOptionalIntegerToEnumValidRoundTripsRelease(t *testing.T) {
	// The same in-range `1 as ?Color` round trip in RELEASE mode: the
	// validity query and the value both behave identically to SAFE, so the
	// unwrapped value still round-trips to 1 (green).
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", 1, false)
}

func TestEmitOptionalIntegerToEnumFromLocalCompilesAndRuns(t *testing.T) {
	// A source value from a local (not a literal): `n as ?Color` where n is
	// an i32 local. In range (n = 2, blue), has_value is true and the unwrap
	// round-trips to 2; out of range (n = 7), has_value is false.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 2;\nvar c ?Color = n as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", false, 2, false)
	emitAndRun(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 7;\nvar c ?Color = n as ?Color;\nif c.has_value { return 99; } else { return 0; }\n}", false, 0, false)
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 2;\nvar c ?Color = n as ?Color;\nif !c.has_value { return 99; }\nreturn c! as i32;\n}", 2, false)
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 7;\nvar c ?Color = n as ?Color;\nif c.has_value { return 99; } else { return 0; }\n}", 0, false)
}

func TestEmitOptionalIntegerToEnumEvaluatesSourceExactlyOnce(t *testing.T) {
	// The actual point of the pre-statement design: the cast must evaluate
	// its source integer exactly ONCE. bump(&count) increments count through
	// the pointer and returns 0; if the source expression were embedded twice
	// (once for the has_value query, once for the value), count would be 2.
	// It must be 1. SAFE mode.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn bump(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 {\nvar count i32 = 0;\nvar c ?Color = bump(&count) as ?Color;\nif !c.has_value { return 99; }\nreturn count;\n}", false, 1, false)
}

func TestEmitOptionalIntegerToEnumEvaluatesSourceExactlyOnceRelease(t *testing.T) {
	// The same single-evaluation guarantee in RELEASE mode — identical
	// behavior, since the temp-hoisting design is mode-independent.
	emitAndRunRelease(t, "type Color = enum { red, green, blue }; fn bump(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 {\nvar count i32 = 0;\nvar c ?Color = bump(&count) as ?Color;\nif !c.has_value { return 99; }\nreturn count;\n}", 1, false)
}

func TestEmitOptionalIntegerToEnumForLoopInitializerCompilesAndRuns(t *testing.T) {
	// The for-loop-initializer position: `for var c ?Color = ...; ...` — the
	// cast is hoisted into an int64_t temp emitted as a statement before the
	// for, and the header's init clause is the optional-typed local reading
	// that temp. In range (1), the condition c.has_value is true so the body
	// breaks and the program returns 1; out of range (5), has_value is false
	// so the loop body never runs and the program returns 0. Bounded
	// execution, in both modes.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 1 as ?Color; c.has_value; { break; }\nreturn 1;\n}", false, 1, false)
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 5 as ?Color; c.has_value; { break; }\nreturn 0;\n}", false, 0, false)
	unit, snapshot, entryID, sources := buildFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nfor var c ?Color = 1 as ?Color; c.has_value; { break; }\nreturn 1;\n}", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileEmittedCRelease(t, buf.Bytes())
}

func TestEmitOptionalIntegerToEnumWritesC(t *testing.T) {
	// Confirm the emitted C directly: the source integer is evaluated exactly
	// once into an int64_t temp (a pre-statement), and the optional local is
	// then constructed with both fields read from that single temp — the
	// has_value bool from the runtime validity query and the enum value
	// narrowed from the temp. The temp name derives from the Initialize
	// node's own id, mirroring pebble_compound_ptr_<id>. The enum typedef is
	// emitted before the optional typedef that names it as the value field.
	unit, snapshot, entryID, enumType, _, sources := enumFixture(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar c ?Color = 1 as ?Color;\nreturn c! as i32;\n}")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if got := strings.Count(out, "int64_t pebble_temp_"); got != 1 {
		t.Errorf("source integer temp appears %d time(s), want exactly one (single evaluation):\n%s", got, out)
	}
	for _, want := range []string{
		"int64_t pebble_temp_",
		".has_value = pebble_rt_int_to_enum_is_valid(pebble_temp_",
		".value = (" + enumTypeName(enumType) + ")pebble_temp_",
		"(" + enumTypeName(enumType) + ")pebble_rt_checked_unwrap_i32(pebble_local_",
		"typedef struct {\n    bool has_value;\n    " + enumTypeName(enumType) + " value;\n} pebble_optional_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	enumIndex := strings.Index(out, "typedef enum {")
	optionalIndex := strings.Index(out, "} pebble_optional_")
	if enumIndex < 0 || optionalIndex < 0 || enumIndex > optionalIndex {
		t.Errorf("enum typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalImplicitInjectionCompilesAndRuns(t *testing.T) {
	// Implicit optional injection (`var o ?int = 5;`, no `some` keyword) is a
	// tir.OptionalInject node, distinct from an explicit tir.SomeOptional —
	// found unimplemented during a real-code audit (checker accepts it,
	// backend previously rejected it by name: "declares an optional-typed
	// local ... initialized from a OptionalInject, want some <expr> or
	// none"). Fixed by handling OptionalInject alongside SomeOptional in
	// buildOptionalLocalDeclaration's switch, since both lower to the
	// identical C. This is scoped to local declarations only, matching
	// SomeOptional's own existing scope — not a claim that implicit
	// injection works everywhere (e.g. an optional function RESULT type is
	// a separate, broader, not-yet-supported restriction, confirmed
	// unrelated to this fix and tracked separately).
	emitAndRun(t, "fn main() int {\nvar o ?int = 5;\nif o.has_value { return 1; } else { return 0; }\n}", false, 1, false)
}

func TestEmitUnionLocalSwitchValueCompilesAndRuns(t *testing.T) {
	// The flagship fixture from the brief: a tagged-union local constructed
	// with a payload (Choice.value(5)) and switched on by discriminant, each
	// case returning a distinct value. The union is emitted as a tagged struct
	// whose tag is the discriminant enum constant; the switch compares
	// pebble_local_<sym>.tag against the case labels, so the value case fires
	// and the exit code is 1.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionLocalSwitchEmptyCompilesAndRuns(t *testing.T) {
	// Same switch, c constructed as the payload-less variant (Choice.empty, an
	// EnumVariantValue): the empty case fires and the exit code is 0, proving
	// the payload-less variant of a tagged union (whose other variant DOES
	// carry a payload elsewhere in the reachable program) still dispatches to
	// its own discriminant case — the payload union is simply left unspecified.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 0, false)
}

func TestEmitUnionSwitchMultiValueCaseCompilesAndRuns(t *testing.T) {
	// A multi-value case on a tagged-union subject: `case Choice.empty,
	// Choice.value:` produces two SwitchCase nodes sharing one body node ID
	// (confirmed against a real fixture), which stack as two C case labels
	// sharing one body exactly as plain-enum and integer multi-value cases do.
	// c = Choice.value(5) hits the multi-value case and returns 10.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty, Choice.value: return 10; }\n}", false, 10, false)
}

func TestEmitUnionSwitchElseCompilesAndRuns(t *testing.T) {
	// An else arm on a tagged-union switch: c = Choice.empty is covered by no
	// case (only value has one), so the else/default arm fires and returns 20.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nswitch c { case Choice.value: return 10; else: return 20; }\n}", false, 20, false)
}

func TestEmitUnionStoreCompilesAndRuns(t *testing.T) {
	// Reassigning a tagged-union local from a payload-less construction to a
	// payload-carrying one (c = Choice.value(5);) lowers through buildStoreCore
	// to the union's compound literal, so the subsequent switch fires the value
	// case. This proves the stored value actually changed, not just that the
	// store compiled.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.empty;\nc = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionBoolPayloadCompilesAndRuns(t *testing.T) {
	// A bool payload variant (Flag.on(true)): the union member is declared bool
	// and the payload expression builds under the bool grammar. The on case
	// fires and the exit code is 1.
	emitAndRun(t, "type Flag = union enum { off void; on bool; }; fn main() i32 {\nvar f Flag = Flag.on(true);\nswitch f { case Flag.off: return 0; case Flag.on: return 1; }\n}", false, 1, false)
}

func TestEmitUnionTwoPayloadVariantsCompilesAndRuns(t *testing.T) {
	// A tagged union with two non-void variants whose payload types differ (one
	// i32, one bool): both union members are declared (int32_t and bool), the
	// construction of each names its own member, and the discriminant alone
	// selects the correct case. Constructing the wide variant and switching
	// fires the wide case (exit 1), independent of the big variant's payload
	// member.
	emitAndRun(t, "type Shape = union enum { empty void; wide i32; big bool; }; fn main() i32 {\nvar a Shape = Shape.wide(42);\nvar b Shape = Shape.big(false);\nswitch a { case Shape.empty: return 0; case Shape.wide: return 1; case Shape.big: return 2; }\n}", false, 1, false)
}

func TestEmitUnionTwoPayloadVariantsOtherCaseCompilesAndRuns(t *testing.T) {
	// Same two-payload-variant union, switching on the bool-constructed local:
	// the big case fires (exit 2), proving the discriminant dispatch reaches
	// the other payload-carrying variant too.
	emitAndRun(t, "type Shape = union enum { empty void; wide i32; big bool; }; fn main() i32 {\nvar a Shape = Shape.wide(42);\nvar b Shape = Shape.big(true);\nswitch b { case Shape.empty: return 0; case Shape.wide: return 1; case Shape.big: return 2; }\n}", false, 2, false)
}

func TestEmitUnionRecordConstructQualifiedCompilesAndRuns(t *testing.T) {
	// The .{ Int = 42 } construction surface for a tagged union, qualified form:
	// finishRecord routes it to an aggregateTaggedVariant record and the IR
	// builder produces the same VariantConstruct node as the call-syntax form
	// Data.Int(42), so the backend lowers it identically and the switch fires
	// the Int case (exit 1), proving the two syntaxes are equivalent.
	emitAndRun(t, "type Data = union enum { Int i32; Str str; }; fn main() i32 {\nvar d Data = Data.{ Int = 42 };\nswitch d { case Data.Int: return 1; case Data.Str: return 0; }\n}", false, 1, false)
}

func TestEmitUnionRecordConstructInferredCompilesAndRuns(t *testing.T) {
	// Same construction via the inferred receiver form (.{ Int = 42 } against an
	// annotated destination): the variant symbol is re-derived by name at IR
	// build time, and the emitted switch still fires the Int case (exit 1).
	emitAndRun(t, "type Data = union enum { Int i32; Str str; }; fn main() i32 {\nvar d Data = .{ Int = 42 };\nswitch d { case Data.Int: return 1; case Data.Str: return 0; }\n}", false, 1, false)
}

func TestEmitUnionVariantLiteralSwitchSubjectCompilesAndRuns(t *testing.T) {
	// A variant construction used directly as the switch subject (switch
	// Choice.value(5)) — confirmed checker-reachable — is built as the union's
	// compound literal and its .tag field read, so the value case fires.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nswitch Choice.value(5) { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 1, false)
}

func TestEmitUnionPayloadRoundTripsThroughConstruction(t *testing.T) {
	// No syntax in the language reads a tagged-union payload back out (a switch
	// case value is a bare expression — there is no pattern-binding syntax, so
	// a case can only ever match by discriminant), so the payload's round-trip
	// is observed through the only channel that exists: construction itself. An
	// anchored payload expression that overflows must be evaluated exactly once
	// and stored at construction time — if the backend dropped or mis-lowered
	// the payload, the overflow would never fire. Constructing
	// Choice.value(x + 1) with x = INT32_MAX must abort at construction, which
	// proves the payload value is genuinely evaluated and stored even though
	// the language cannot read it back.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nlet x i32 = 2147483647;\nvar c Choice = Choice.value(x + 1);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}", false, 0, true)
}

func TestEmitUnionAllVoidVariantsCompilesAndRuns(t *testing.T) {
	// A tagged union every variant of which is payload-less (union enum { a
	// void; b void; } — legal per the grammar, since `union enum` merely marks
	// the tagged form) reaches this backend with no payload-carrying
	// construction anywhere, so nothing in 10.34's plain-enum code path changes
	// for it: the type is emitted as a plain enum typedef exactly like an
	// `enum` declaration, and the switch dispatches on the bare enum value.
	// This test proves the all-void union case needs no new implementation.
	emitAndRun(t, "type Empty = union enum { a void; b void; }; fn main() i32 {\nvar e Empty = Empty.b;\nswitch e { case Empty.a: return 0; case Empty.b: return 1; }\n}", false, 1, false)
}

func TestEmitUnionSwitchInHelperCompilesAndRuns(t *testing.T) {
	// A tagged-union local and switch inside a reachable helper, the entry
	// calling the helper: collectUnionTypes walks every reachable helper's
	// body, so the union's typedef pair is discovered and emitted even when no
	// reachable *entry* statement constructs a payload variant.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn pick() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}\nfn main() i32 { return pick(); }", false, 1, false)
}

func TestEmitUnionLocalUnusedCompilesClean(t *testing.T) {
	// A tagged-union local declared and never referenced after its declaration
	// still compiles clean under -Wall -Wextra -Werror: the emitted declaration
	// is followed by the same (void) cast every other local gets, so the strict
	// build never warns about an unused variable.
	emitAndRun(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nreturn 3;\n}", false, 3, false)
}

func TestEmitUnionWritesC(t *testing.T) {
	// Confirm the emitted C directly for one fixture: the tag-enum typedef
	// (one constant per variant in declared order), the tagged-struct typedef
	// with its union member(s) named pebble_field_<member> from each
	// constructed variant's symbol, the local declaration initializing from
	// the construction's compound literal (tag + designated payload member),
	// and the switch subject reading the .tag field.
	unit, snapshot, entryID, unionType, variants, sources := unionFixture(t, "type Choice = union enum { empty void; value i32; }; fn main() i32 {\nvar c Choice = Choice.value(5);\nswitch c { case Choice.empty: return 0; case Choice.value: return 1; }\n}")
	if len(variants) != 2 {
		t.Fatalf("fixture has %d variants, want 2 (empty, value)", len(variants))
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	tagEnum := "typedef enum {\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[0])) + ",\n" +
		"    pebble_variant_" + strconv.Itoa(int(variants[1])) + ",\n" +
		"} " + enumTypeName(unionType) + ";"
	if !strings.Contains(out, tagEnum) {
		t.Errorf("emitted C is missing the tag enum typedef %q:\n%s", tagEnum, out)
	}
	unionStruct := "typedef struct {\n" +
		"    " + enumTypeName(unionType) + " tag;\n" +
		"    union {\n" +
		"        int32_t pebble_field_" + strconv.Itoa(int(variants[1])) + ";\n" +
		"    } payload;\n" +
		"} " + unionTypeName(unionType) + ";"
	if !strings.Contains(out, unionStruct) {
		t.Errorf("emitted C is missing the tagged-struct typedef %q:\n%s", unionStruct, out)
	}
	for _, want := range []string{
		unionTypeName(unionType) + " pebble_local_",
		"(pebble_union_" + strconv.Itoa(int(unionType)) + "_t){ .tag = pebble_variant_" + strconv.Itoa(int(variants[1])) + ", .payload = { .pebble_field_" + strconv.Itoa(int(variants[1])) + " = 5 } }",
		"switch (pebble_local_",
		".tag) {",
		"case pebble_variant_" + strconv.Itoa(int(variants[0])) + ":",
		"case pebble_variant_" + strconv.Itoa(int(variants[1])) + ":",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitNarrowedUnionVariantPayloadReadCompilesAndRuns(t *testing.T) {
	// Slice C's end-to-end program: a narrowed union-variant payload read
	// (`r.Ok` inside `case .Ok:`) must emit the payload projection the union's
	// construction side fills — pebble_local_<sym>.payload.pebble_field_<m> —
	// inside a helper whose union-typed parameter is passed by value. The
	// union also carries a str-typed payload (Err = "bad"), so the union
	// payload gate now admits str (PebbleStr member). unwrap_or(a, 0) reads
	// the Ok payload 42; unwrap_or(b, 100) falls to its def; 42 + 100 = 142.
	emitAndRun(t, `type Result[T, E] = union enum {
    Ok T;
    Err E;
};
fn unwrap_or(r Result[int, str], def int) int {
    switch r {
        case .Ok: return r.Ok;
        case .Err: return def;
    }
}
fn main() int {
    let a = Result[int, str].{ Ok = 42 };
    let b = Result[int, str].{ Err = "bad" };
    return unwrap_or(a, 0) + unwrap_or(b, 100);
}`, false, 142, false)
}

func TestEmitTaggedUnionStructFieldCompilesAndRuns(t *testing.T) {
	// The tagged-union-as-struct-field fix (proposal 13, case 1), proven end to
	// end: a struct whose field type is a tagged union must both (a) declare
	// the field with the union's own pebble_union_<typeID>_t typedef — never
	// the bare tag-enum pebble_enum_<typeID>_t — and (b) emit that union
	// typedef before the struct typedef that references it, or cc rejects the
	// emitted C. The round trip is the real proof, not just clean emission: the
	// payload-carrying variant Choice.value(42) is constructed, stored into the
	// Holder.tag field, read back out (h.tag), and the payload 42 is recovered
	// through a narrowing switch on a function parameter — the working switch
	// position (a same-scope let-bound switch subject is a separate, unrelated
	// tracker gap that this test deliberately avoids). The exit code is 42 only
	// if the value survives construction -> storage -> field read -> narrowing
	// switch intact.
	emitAndRun(t, `type Choice = union enum {
    empty void;
    value int;
};
type Holder = struct {
    tag Choice;
};
fn readBack(c Choice) int {
    switch c {
        case Choice.empty: return -1;
        case Choice.value: return c.value;
    }
}
fn main() int {
    let c = Choice.value(42);
    var h = Holder.{ tag = c };
    return readBack(h.tag);
}`, false, 42, false)
}

func TestEmitTaggedUnionOptionalPayloadCompilesAndRuns(t *testing.T) {
	// The tagged-union-as-optional-payload fix (proposal 13, case 2), proven
	// end to end: an optional whose payload is a tagged union must declare its
	// .value field with the union's own pebble_union_<typeID>_t typedef, and a
	// `some <union>` construction must store the full union value into it. The
	// round trip: the payload-carrying variant Choice.value(42) is constructed,
	// stored into the ?Choice optional, force-unwrapped (o!), and the payload
	// 42 is recovered through a narrowing switch on a function parameter. The
	// exit code is 42 only if the value survives construction -> storage ->
	// unwrap -> narrowing switch intact; the has_value guard plus the -2
	// fallback prove the some branch (not a vacuous path) produced it.
	emitAndRun(t, `type Choice = union enum {
    empty void;
    value int;
};
fn readBack(c Choice) int {
    switch c {
        case Choice.empty: return -1;
        case Choice.value: return c.value;
    }
}
fn main() int {
    let c = Choice.value(42);
    var o ?Choice = some c;
    if o.has_value {
        return readBack(o!);
    }
    return -2;
}`, false, 42, false)
}

func TestEmitTaggedUnionOptionalUnwrapNonePanics(t *testing.T) {
	// The union-payload optional force-unwrap is a checked operation, not a
	// bare field read: force-unwrapping a `none`-carrying ?Choice must panic
	// with the runtime's unwrap-of-empty-optional panic (PEBBLE_PANIC_UNWRAP_
	// FAILED), the same failure the scalar unwrap helpers raise — the inline
	// ternary's absent branch is dead only when has_value is true, so this
	// proves the panic path is wired, not just that the some path round-trips.
	// The let c = Choice.value(42) construction makes Choice a tagged union in
	// this program (so the optional is a union-payload optional, not a plain
	// enum payload), while the optional itself holds none.
	emitAndRun(t, `type Choice = union enum {
    empty void;
    value int;
};
fn readBack(c Choice) int {
    switch c {
        case Choice.empty: return -1;
        case Choice.value: return c.value;
    }
}
fn main() int {
    let c = Choice.value(42);
    var o ?Choice = none;
    if o.has_value { return 99; }
    return readBack(o!);
}`, false, 0, true)
}

func TestEmitTaggedUnionStructFieldWritesC(t *testing.T) {
	// Emitted-C shape check for the struct-field fix, locking in the typedef
	// ORDERING that is bug 1: the union's typedef pair (the discriminant
	// pebble_enum_<typeID>_t followed by the tagged pebble_union_<typeID>_t
	// struct) must appear in the output BEFORE the struct typedef that
	// references the union as a field type — C requires a type fully defined
	// before use, and this ordering was the raw cc failure before the fix. And
	// the struct field must be declared with the union's own typedef name
	// (pebble_union_<typeID>_t pebble_field_<m>;), never the bare tag-enum
	// pebble_enum_<typeID>_t — bug 2's wrong type.
	unit, snapshot, entryID, unionType, _, sources := unionFixture(t, `type Choice = union enum {
    empty void;
    value int;
};
type Holder = struct {
    tag Choice;
};
fn main() int {
    let c = Choice.value(42);
    var h = Holder.{ tag = c };
    return 0;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	unionClose := "} " + unionTypeName(unionType) + ";"
	unionCloseIdx := strings.Index(out, unionClose)
	fieldUse := unionTypeName(unionType) + " pebble_field_"
	fieldUseIdx := strings.Index(out, fieldUse)
	if unionCloseIdx < 0 {
		t.Errorf("emitted C missing the union typedef close %q:\n%s", unionClose, out)
	}
	if fieldUseIdx < 0 {
		t.Errorf("emitted C missing the union-typed struct field declaration %q (the field must be typed with the union's own typedef):\n%s", fieldUse, out)
	}
	if unionCloseIdx >= 0 && fieldUseIdx >= 0 && unionCloseIdx > fieldUseIdx {
		t.Errorf("emitted C defines the struct typedef (%q at index %d) BEFORE the union typedef it references (%q at index %d); the union typedef must precede the struct typedef:\n%s", fieldUse, fieldUseIdx, unionClose, unionCloseIdx, out)
	}
	if strings.Contains(out, enumTypeName(unionType)+" pebble_field_") {
		t.Errorf("emitted C declares the tagged-union struct field with the bare tag-enum typedef %q, want the union's own typedef %q:\n%s", enumTypeName(unionType)+" pebble_field_", unionTypeName(unionType)+" pebble_field_", out)
	}
}

func TestEmitTaggedUnionOptionalPayloadWritesC(t *testing.T) {
	// Emitted-C shape check for the optional-payload fix: the optional struct's
	// .value field must be declared with the union's own
	// pebble_union_<typeID>_t typedef — never the bare tag-enum
	// pebble_enum_<typeID>_t — and the union's typedef pair must precede the
	// optional typedef that references it (the same C define-before-use
	// ordering bug 1 fixed for struct fields).
	unit, snapshot, entryID, unionType, _, sources := unionFixture(t, `type Choice = union enum {
    empty void;
    value int;
};
fn main() int {
    let c = Choice.value(42);
    var o ?Choice = some c;
    return 0;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	unionClose := "} " + unionTypeName(unionType) + ";"
	unionCloseIdx := strings.Index(out, unionClose)
	valueField := unionTypeName(unionType) + " value;"
	valueFieldIdx := strings.Index(out, valueField)
	if unionCloseIdx < 0 {
		t.Errorf("emitted C missing the union typedef close %q:\n%s", unionClose, out)
	}
	if valueFieldIdx < 0 {
		t.Errorf("emitted C missing the union-typed optional value field %q (the payload must be typed with the union's own typedef):\n%s", valueField, out)
	}
	if unionCloseIdx >= 0 && valueFieldIdx >= 0 && unionCloseIdx > valueFieldIdx {
		t.Errorf("emitted C defines the optional typedef (%q at index %d) BEFORE the union typedef it references (%q at index %d); the union typedef must precede the optional typedef:\n%s", valueField, valueFieldIdx, unionClose, unionCloseIdx, out)
	}
	if strings.Contains(out, enumTypeName(unionType)+" value;") {
		t.Errorf("emitted C declares the tagged-union optional value field with the bare tag-enum typedef %q, want the union's own typedef %q:\n%s", enumTypeName(unionType)+" value;", unionTypeName(unionType)+" value;", out)
	}
}

func TestEmitEnumSwitchInHelperCompilesAndRuns(t *testing.T) {
	// A plain enum local and switch inside a reachable helper, the entry
	// calling the helper: collectEnumTypes walks every reachable helper's body,
	// so the enum typedef is discovered and emitted even when no reachable
	// *entry* statement references the enum type. The helper's switch on its
	// own enum local fires the green case and returns 1.
	emitAndRun(t, "type Color = enum { red, green, blue }; fn pick() i32 {\nvar c Color = Color.green;\nswitch c { case Color.red: return 0; case Color.green: return 1; case Color.blue: return 2; }\n}\nfn main() i32 { return pick(); }", false, 1, false)
}

func TestEmitEnumLocalInLoopBodyCompilesAndRuns(t *testing.T) {
	// An enum-typed local declared inside a while loop body, reassigned and
	// compared there, and accumulated: the enum dispatch routes through
	// buildLeadingStatement from buildLoopBody exactly as a scalar local does.
	// i = 0 and i = 1 leave c as green, each adding 1 (n = 2); i = 2 reassigns
	// c to red, whose equality comparison is true and adds 10 (n = 12). Bounded
	// execution because the loop's own condition is the only bound.
	emitAndRunBounded(t, "type Color = enum { red, green, blue }; fn main() i32 {\nvar n i32 = 0;\nvar i i32 = 0;\nwhile i < 3 {\nvar c Color = Color.green;\nif i == 2 { c = Color.red; }\nif c == Color.red { n = n + 10; } else { n = n + 1; }\ni = i + 1;\n}\nreturn n;\n}", false, 12, false)
}

func TestEmitSliceBothBoundsCompilesAndRuns(t *testing.T) {
	// Slice from array with both bounds explicit: a[1:3] from [1,2,3,4,5]
	// gives elements [2,3]; s[0] should be 2.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[0]; }", false, 2, false)
}

func TestEmitSliceStartOnlyCompilesAndRuns(t *testing.T) {
	// Slice with only start bound: a[2:] from [10,20,30,40,50] gives
	// elements [30,40,50]; s[0] should be 30.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[2:]; return s[0]; }", false, 30, false)
}

func TestEmitSliceEndOnlyCompilesAndRuns(t *testing.T) {
	// Slice with only end bound: a[:3] from [10,20,30,40,50] gives
	// elements [10,20,30]; s[0] should be 10.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[:3]; return s[0]; }", false, 10, false)
}

func TestEmitSliceNoBoundsCompilesAndRuns(t *testing.T) {
	// Slice with no bounds: a[:] from [10,20,30,40,50] gives all 5 elements;
	// s[2] should be 30.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [10, 20, 30, 40, 50]; var s []i32 = a[:]; return s[2]; }", false, 30, false)
}

func TestEmitSliceBoolElementCompilesAndRuns(t *testing.T) {
	// Bool-element slice: a[1:3] from [true, false, true, false] gives
	// [false, true]; s[0] is false, so if s[0] { return 1 } else { return 0 }
	// returns 0; s[1] is true, so if s[1] { return 1 } else { return 0 }
	// returns 1. Use the slice in an expression that drives the return.
	emitAndRun(t, "fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; if s[1] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitSliceI64CompilesAndRuns(t *testing.T) {
	// i64-entry slice: a[1:3] from [100,200,300,400,500] gives [200,300];
	// s[0] should be 200.
	emitAndRun(t, "fn main() i64 { var a [5]i64 = [100, 200, 300, 400, 500]; var s []i64 = a[1:3]; return s[0]; }", false, 200, false)
}

func TestEmitU8SliceFromArrayCompilesAndRuns(t *testing.T) {
	// []u8, an entry-width-independent fixed-width integer element (main
	// returns int, not u8): construct a [3]u8 array, slice it, index-read
	// element 1. Confirms both the array-typed-local element gate and the
	// slice-construction/index-read paths accept a non-entry-width scalar.
	emitAndRun(t, "fn main() int { var arr [3]u8 = [1 as u8, 2 as u8, 3 as u8]; var s []u8 = arr[:]; return s[1] as int; }", false, 2, false)
}

func TestEmitCharSliceFromArrayCompilesAndRuns(t *testing.T) {
	// []char, mirroring the u8 case above for char specifically (its own
	// fixed int32_t C representation, not a resolvedBuiltin integer).
	emitAndRun(t, "fn main() int { var arr [3]char = ['a', 'b', 'c']; var s []char = arr[:]; if s[1] == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitI64SliceNonEntryWidthCompilesAndRuns(t *testing.T) {
	// Unlike TestEmitSliceI64CompilesAndRuns (where i64 IS the entry width,
	// since main returns i64 there), this entry returns int, so i64 here is
	// genuinely a non-ambient width — the case that previously only worked
	// by coincidence when i64 happened to match the entry's own width.
	emitAndRun(t, "fn main() int { var arr [3]i64 = [100, 200, 300]; var s []i64 = arr[:]; return s[1] as int; }", false, 200, false)
}

func TestEmitU64SliceConstructionInHelperCompilesAndRuns(t *testing.T) {
	// The slice-start gap this slice closes: constructing an ordinary INT
	// slice (arr[:]) inside a u64-returning helper — the u64-ness comes from
	// the AMBIENT function width, not the slice's own element type — must
	// lower to pebble_rt_checked_slice_start_u64 (not the empty-suffix name
	// that previously failed at cc compile time, nor a nonexistent helper).
	// The slice must build with correct bounds and its element read correctly,
	// called from an int-entry main.
	emitAndRun(t, "fn f() u64 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; return s[1] as u64; } fn main() int { return f() as int; }", false, 2, false)
}

func TestEmitU64SliceConstructionOutOfBoundsAborts(t *testing.T) {
	// The out-of-bounds twin of the slice-start fix: a runtime end bound past
	// the array length inside a u64-returning helper must route through
	// pebble_rt_checked_slice_start_u64 and panic with
	// PEBBLE_PANIC_INDEX_OUT_OF_BOUNDS in every configuration, so the process
	// terminates abnormally. A helper supplies the out-of-range end to bypass
	// the checker's compile-time range validation.
	emitAndRun(t, "fn getEnd() u64 { return 10; } fn f() u64 { var arr [3]int = [1, 2, 3]; var e u64 = getEnd(); var s []int = arr[0:e]; return s[0] as u64; } fn main() int { return f() as int; }", false, 0, true)
}

func TestEmitU64SliceConstructionWritesU64Helper(t *testing.T) {
	// Assert the exact helper name in the emitted C: a slice construction
	// inside a u64-returning helper must call pebble_rt_checked_slice_start_u64.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() u64 { var arr [3]int = [1, 2, 3]; var s []int = arr[:]; return s[1] as u64; } fn main() int { return f() as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_slice_start_u64(") {
		t.Errorf("emitted C missing the u64 checked-slice-start helper:\n%s", out)
	}
}

func TestEmitSliceOutOfBoundsRangeAborts(t *testing.T) {
	// Out-of-range slice end bound: use a helper to supply a runtime end
	// value that exceeds the array length, bypassing the checker's
	// compile-time validation. pebble_rt_checked_slice_start_i32 must panic.
	emitAndRun(t, "fn getEnd() i32 { return 10; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var e i32 = getEnd(); var s []i32 = a[0:e]; return s[0]; }", false, 0, true)
}

func TestEmitSliceIndexOutOfBoundsAborts(t *testing.T) {
	// Out-of-range index into a valid slice: a[1:3] gives 2 elements [2,3];
	// s[5] is out of bounds, triggering pebble_rt_checked_index_i32.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[5]; }", false, 0, true)
}

func TestEmitSliceRangeOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, the checked slice-range construction call also carries a
	// real, resolved Pebble source location (the CheckedSlice node's own
	// Span) instead of the zero-valued placeholder. This proves both that the
	// emitted C no longer uses the placeholder anywhere and that the runtime
	// still aborts on an invalid range (end bound past the array length).
	unit, snapshot, entryID, sources := buildFixture(t, "fn getEnd() i32 { return 10; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var e i32 = getEnd(); var s []i32 = a[0:e]; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_slice_start_i32(") {
		t.Errorf("emitted C missing the checked slice-start call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitSliceIndexOutOfBoundsEmitsRealSourceLoc(t *testing.T) {
	// Since 10.44, an out-of-bounds read through a slice (s[5] on a 2-element
	// slice) also carries a real, resolved Pebble source location on its
	// checked-index call instead of the zero-valued placeholder.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[5]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_index_i32(") {
		t.Errorf("emitted C missing the checked-index call:\n%s", out)
	}
	if strings.Contains(out, "(PebbleSourceLoc){0}") {
		t.Errorf("emitted C still uses the zero-valued source-location placeholder:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, true)
}

func TestEmitSliceEmittedCDirectly(t *testing.T) {
	// Confirm the emitted C directly: slice typedef shape, construction
	// compound-literal text (including inline checked-start call), and
	// indexing expression (including inline checked-index call and .len cast).
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Slice typedef must be present.
	if !strings.Contains(out, "typedef struct {") {
		t.Errorf("emitted C missing slice typedef:\n%s", out)
	}
	if !strings.Contains(out, "int32_t *data;") {
		t.Errorf("emitted C missing data field in slice typedef:\n%s", out)
	}
	if !strings.Contains(out, "size_t len;") {
		t.Errorf("emitted C missing len field in slice typedef:\n%s", out)
	}
	// Construction must contain pebble_rt_checked_slice_start_i32.
	if !strings.Contains(out, "pebble_rt_checked_slice_start_i32") {
		t.Errorf("emitted C missing checked-slice-start call:\n%s", out)
	}
	// Indexing must contain pebble_rt_checked_index_i32 and .data and .len.
	if !strings.Contains(out, "pebble_rt_checked_index_i32") {
		t.Errorf("emitted C missing checked-index call for slice indexing:\n%s", out)
	}
	if !strings.Contains(out, ".data[") {
		t.Errorf("emitted C missing .data subscript for slice indexing:\n%s", out)
	}
	if !strings.Contains(out, ".len") {
		t.Errorf("emitted C missing .len in slice indexing:\n%s", out)
	}
	// Run end-to-end to confirm correctness.
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceOfStructElementsCompilesAndRuns(t *testing.T) {
	// The standalone synthetic repro of the slice-of-struct-elements gap: a
	// []P slice constructed from a [3]P array via arr[:], then every index
	// shape std/hmap.peb's real insert/get/rehash use — a field write through a
	// slice index (s[1].state = .Occ, matching new_entries[i].state = .Empty),
	// an address-of slice index for in-place field mutation (let p *P = &s[2];
	// p.x = 50, matching `let entry = &self.entries[index]; entry.state =
	// .Occupied`), and a by-value element read into a struct local (let e =
	// s[1]; e.state == .Occ, matching `let e = old_entries[j];`) — compiles and
	// runs correctly. The exit code depends on all of them working: 30 (s[1].x
	// after the s[1].x = 30 write) + 50 (s[2].x after the pointer mutation) =
	// 80.
	emitAndRun(t, "type S = enum { Empty, Occ };\ntype P = struct { x i32; y i32; state S; };\nfn main() i32 {\nvar arr [3]P = [P.{ x = 1, y = 2, state = .Empty }, P.{ x = 3, y = 4, state = .Empty }, P.{ x = 5, y = 6, state = .Empty }];\nvar s []P = arr[:];\ns[1].state = .Occ;\ns[1].x = 30;\nlet p *P = &s[2];\np.x = 50;\np.state = .Occ;\nlet e = s[1];\nif e.state == .Occ { return e.x + s[2].x; }\nreturn 0;\n}", false, 80, false)
}

func TestEmitSliceOfStructElementsEmittedCShape(t *testing.T) {
	// The emitted-C shape check for a struct-element slice: the slice typedef's
	// .data field must be a pointer to the struct's OWN typedef name
	// (pebble_struct_<typeID>_t *data), not a rejection; the element struct's
	// typedef must be forward-declared BEFORE the slice typedef (C requires the
	// pointer target's name declared, even incompletely, before the slice block
	// references it — the slice block is emitted before the aggregate block
	// that fully defines the struct); and the struct's full definition must
	// carry the matching struct tag so the forward declaration and the
	// definition complete the same C type.
	unit, snapshot, entryID, sources := buildFixture(t, "type P = struct { x i32; y i32; };\nfn main() i32 { var a [3]P = [P.{ x = 1, y = 2 }, P.{ x = 3, y = 4 }, P.{ x = 5, y = 6 }]; var s []P = a[:]; return s[1].x + s[1].y; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The slice typedef's data field must be a pointer to the struct's own
	// typedef name.
	fwdDecl := "typedef struct pebble_struct_"
	fwdIdx := strings.Index(out, fwdDecl)
	if fwdIdx < 0 {
		t.Fatalf("emitted C missing the struct forward typedef declaration:\n%s", out)
	}
	// Extract the struct typedef name from the forward declaration
	// `typedef struct pebble_struct_<id> pebble_struct_<id>_t;`.
	rest := out[fwdIdx+len(fwdDecl):]
	tagEnd := strings.Index(rest, " ")
	if tagEnd < 0 {
		t.Fatalf("emitted C malformed forward typedef declaration:\n%s", out)
	}
	structTag := "pebble_struct_" + rest[:tagEnd]
	structName := structTag + "_t"
	if !strings.Contains(out, structName+" *data;") {
		t.Fatalf("emitted C slice typedef's .data field is not a pointer to the struct's own typedef name %s:\n%s", structName, out)
	}
	// The struct's typedef name must be forward-declared before the slice
	// typedef that points at it.
	sliceIdx := strings.Index(out, "pebble_slice_")
	fwd := "typedef struct " + structTag + " " + structName + ";"
	if sliceIdx < 0 || !strings.Contains(out[:sliceIdx], fwd) {
		t.Fatalf("emitted C does not forward-declare the struct typedef before the slice typedef:\n%s", out)
	}
	// The struct's full definition must carry the matching tag.
	tag := "typedef struct " + structTag
	if !strings.Contains(out, tag) {
		t.Fatalf("emitted C struct definition missing the matching struct tag %s:\n%s", tag, out)
	}
	compileAndRun(t, buf.Bytes(), 7, false)
}

func TestEmitSliceOfStructElementsFromRawStdCompilesAndRuns(t *testing.T) {
	// The SliceFromRaw shape (`slice ptr, n`) with a struct element — how
	// std/hmap.peb's rehash/with_capacity actually construct
	// `let entries []Entry[K, V] = slice ptr, cap;` over an allocator-returned
	// pointer. SliceFromRaw is checker-restricted to the standard library
	// package (a C0619 "slice is restricted to the standard library package"
	// rejection for user modules), so this fixture builds in the std package
	// like the existing TestEmitSliceFromRawCompilesAndRuns. The slice's data
	// field points at a contiguous [3]P array (via &arr[0]) and indexes over
	// it, so the exit code proves the whole SliceFromRaw construction plus a
	// struct-element index read work.
	unit, snapshot, entryID, sources := buildStdFixture(t, "type P = struct { x i32; y i32; };\nfn main() i32 { var arr [3]P = [P.{ x = 1, y = 2 }, P.{ x = 3, y = 4 }, P.{ x = 5, y = 6 }]; let s []P = slice &arr[0], 3; return s[2].x + s[2].y; }", "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 11, false)
}

func TestEmitSliceOfTupleElementsCompilesAndRuns(t *testing.T) {
	// A tuple-element slice, the sibling of the struct-element widening: the
	// slice element gate (sliceElementCType / isSupportedSliceElementType)
	// accepts tuples exactly as arrays already do, so `var s [](i32, i32) =
	// a[0:2]` over a [3](i32, i32) array constructs a slice whose typedef's
	// data field points at the tuple's own typedef, and an indexed element
	// read projects through the tuple's positional field (s[1].0). The old
	// rejection of tuple-element slices is deliberately lifted (this exact
	// fixture used to be TestEmitSliceUnsupportedElementTypeRejects); the
	// exit code 3 = s[1].0 proves the whole construction-and-read path.
	emitAndRun(t, "fn main() i32 { var a [3](i32, i32) = [(1, 2), (3, 4), (5, 6)]; var s [](i32, i32) = a[0:2]; return s[1].0; }", false, 3, false)
}

func TestEmitSliceParameterCompilesAndRuns(t *testing.T) {
	// The flagship slice-parameter fixture: first takes a []i32 parameter and
	// indexes it inside the helper; the entry slices an array into a slice
	// local and passes that local. s = a[1:3] = [2,3], so s[0] = 2 is the exit
	// code. The parameter seeds the callee's scope as a slice local and the
	// index resolves through the same Load(CheckedIndexPlace) machinery a
	// slice local uses.
	emitAndRun(t, "fn first(s []i32) i32 { return s[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return first(s); }", false, 2, false)
}

func TestEmitUintBoundedRangeLoopReadsSliceLenCompilesAndRuns(t *testing.T) {
	// The exact shape examples/slice_minmax.peb uses (and the regression test
	// for the emit gap that file surfaced): a uint-bounded range loop whose end
	// bound is a slice's `.len` structural field — `loop 1..items.len : iter {`
	// — with the uint-typed iterator used to index the same slice inside the
	// body (`items[iter]`). The `.len` bound lowers to Load(FieldPlace) with
	// the StructuralFieldLen sentinel member; before the fix, buildPlaceLValue
	// fell through to declaredFieldType and failed with "field 4294967295 is
	// not declared". The loop's start `1` is anchored to the uint end bound, so
	// the iterator is a uint64_t C counter and the body index routes through
	// the uint index path. sub = arr[1:4] = [7, 99, 12], whose min is 7 — the
	// exit code — proving the loop actually iterates the slice at runtime, not
	// just that it emits.
	emitAndRun(t, "fn find_min(items []int) int { var min_val = items[0]; loop 1..items.len : iter { if items[iter] < min_val { min_val = items[iter]; } } return min_val; } fn main() int { var arr [5]int = [42, 7, 99, 12, 3]; var sub []int = arr[1:4]; return find_min(sub); }", false, 7, false)
}

func TestEmitSliceParameterBoolElementCompilesAndRuns(t *testing.T) {
	// A bool-element slice parameter: the element-typed index read routes
	// through the slice's bool element and drives the return. s = a[1:3] =
	// [false, true], so s[0] is false and the else arm exits 0.
	emitAndRun(t, "fn first(s []bool) i32 { if s[0] { return 1; } else { return 0; } } fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; return first(s); }", false, 0, false)
}

func TestEmitSliceParameterI64CompilesAndRuns(t *testing.T) {
	// The width-generic path holds for slice parameters too, mirroring 10.37's
	// own i64 test: an i64 entry calls an i64 slice-taking helper whose slice
	// parameter seeds the callee's scope; s = a[1:3] = [200,300], s[0] = 200 is
	// the exit code. The parameter's C type is pebble_slice_<id>_t with an
	// int64_t* data field.
	emitAndRun(t, "fn first(s []i64) i64 { return s[0]; } fn main() i64 { var a [5]i64 = [100, 200, 300, 400, 500]; var s []i64 = a[1:3]; return first(s); }", false, 200, false)
}

func TestEmitSliceReturningHelperInlineConstructionCompilesAndRuns(t *testing.T) {
	// The flagship slice-return fixture: view's tail return constructs the
	// slice inline (`return a[1:3];` — the Return child is a bare CheckedSlice,
	// confirmed against a real fixture), so the return needs the same
	// two-statement temp-then-construction shape a slice local's declaration
	// uses. The caller declares a slice local from the call (the supported
	// position) and indexes the result: view() = [2,3], s[0] = 2 is the exit
	// code. This confirms the two-statement return construction actually works
	// at runtime, not just that it compiles.
	emitAndRun(t, "fn view() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return a[1:3]; } fn main() i32 { var s []i32 = view(); return s[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperForwardsParameterCompilesAndRuns(t *testing.T) {
	// A slice-returning helper forwarding its slice-typed parameter unchanged
	// (`return s;` — a plain SymbolValue return, the single-statement path):
	// echo passes its parameter back, the entry declares a slice local from the
	// call and indexes it. echo(s) = [2,3], t[0] = 2 is the exit code.
	emitAndRun(t, "fn echo(s []i32) []i32 { return s; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; var t []i32 = echo(s); return t[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperForwardsLocalCompilesAndRuns(t *testing.T) {
	// The local side of forwarding an already-declared slice value: g declares
	// its own array and slice local and `return s;` forwards the local (a
	// plain SymbolValue), emitting `return pebble_local_<s>;`. The entry
	// assigns the call to a matching slice local and indexes it: g() = [2,3],
	// t[0] = 2 is the exit code.
	emitAndRun(t, "fn g() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return s; } fn main() i32 { var t []i32 = g(); return t[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperI64CompilesAndRuns(t *testing.T) {
	// The i64 side of the return-side construction: the return temp must be
	// declared at int64_t (a width bug in exactly this spot was found and fixed
	// during 10.37's review), and the caller indexes the returned slice.
	// view() = [200,300], s[0] = 200 is the exit code.
	emitAndRun(t, "fn view() []i64 { var a [5]i64 = [100, 200, 300, 400, 500]; return a[1:3]; } fn main() i64 { var s []i64 = view(); return s[0]; }", false, 200, false)
}

func TestEmitSliceReturningHelperIfElseTailsCompilesAndRuns(t *testing.T) {
	// Two slice-construction returns in the two arms of an if/else tail: each
	// arm's Return child is a bare CheckedSlice, each built with its own temp
	// (named from the return value node's NodeID, so the two sibling-block
	// temps never collide even though they are the same slice type). With the
	// flag true the then-arm wins: a[0:2] = [1,2], s[0] = 1 is the exit code.
	emitAndRun(t, "fn pick(b bool) []i32 { if b { var a [3]i32 = [1, 2, 3]; return a[0:2]; } else { var a [3]i32 = [4, 5, 6]; return a[1:2]; } } fn main() i32 { var s []i32 = pick(true); return s[0]; }", false, 1, false)
}

func TestEmitSliceReturningHelperSwitchCasesCompilesAndRuns(t *testing.T) {
	// A slice-returning helper whose body tail is a switch whose case bodies
	// are bare single-statement returns of fresh slice constructions: each case
	// body routes through buildSwitchCaseBody's bare-Return slice path and
	// emits its own temp-then-return pair, each temp named from its own return
	// value node's NodeID. With the subject 1 the case-1 body wins: a[1:2] =
	// [2], s[0] = 2 is the exit code.
	emitAndRun(t, "fn pick(i i32) []i32 { var a [3]i32 = [1, 2, 3]; switch i { case 0: return a[0:1]; case 1: return a[1:2]; else: return a[2:3]; } } fn main() i32 { var s []i32 = pick(1); return s[0]; }", false, 2, false)
}

func TestEmitSliceReturningHelperWritesC(t *testing.T) {
	// The emitted C for the inline-construction-return fixture: the helper's C
	// signature declares its return type as the slice typedef, its body emits
	// the two-statement shape (a temp declaration holding the checked-start
	// result, then the return of the compound-literal construction using that
	// temp for both .data and .len), and the call site declares the entry's
	// slice local directly from the call. Symbols 24 (view), 26 (its a array
	// local), 27 (the entry's s local), return value node 18, and slice type 23
	// come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn view() []i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return a[1:3]; } fn main() i32 { var s []i32 = view(); return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t *data;\n    size_t len;\n} pebble_slice_23_t;",
		"static pebble_slice_23_t pebble_fn_24(PebbleContext *ctx) {",
		"int32_t pebble_slice_ret_18 = pebble_rt_checked_slice_start_i32(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_23_t){ .data = pebble_local_28 + pebble_slice_ret_18, .len = (size_t)(3 - pebble_slice_ret_18) };",
		"pebble_slice_23_t pebble_local_29 = pebble_fn_24(ctx);",
		"return pebble_local_29.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_29.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceReturningHelperI64WritesC(t *testing.T) {
	// The i64 counterpart of the return construction, confirming specifically
	// that the return-side temp is declared at int64_t (the exact spot where a
	// width bug was found and fixed during 10.37's review) and then running the
	// emitted C end-to-end.
	unit, snapshot, entryID, sources := buildFixture(t, "fn view() []i64 { var a [5]i64 = [100, 200, 300, 400, 500]; return a[1:3]; } fn main() i64 { var s []i64 = view(); return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int64_t *data;",
		"int64_t pebble_slice_ret_18 = pebble_rt_checked_slice_start_i64(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_23_t){ .data = pebble_local_28 + pebble_slice_ret_18, .len = (size_t)(3 - pebble_slice_ret_18) };",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 200, false)
}

func TestEmitReSliceSliceFieldDefaultEndCompilesAndRuns(t *testing.T) {
	// Re-slicing an EXISTING slice-typed struct field with no start bound and
	// a runtime end — the exact String::as_slice() shape
	// (`return self.data[:self.len];` in std/string.peb). The CheckedSlice's
	// base is a Load(FieldPlace) reading the slice-typed field (NOT a bare
	// SymbolValue naming an array local), which buildSliceConstruction must
	// accept: the new slice's .data offsets the base slice's OWN .data
	// pointer, and the bounds-check helper's upper bound is the base slice's
	// RUNTIME .len field. arr = [10,20,30,40], self.data[:3] = [10,20,30], so
	// s[2] = 30 is the exit code.
	emitAndRun(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 3 };
    var s []int = view(b);
    return s[2];
}`, false, 30, false)
}

func TestEmitReSliceSliceFieldExplicitStartCompilesAndRuns(t *testing.T) {
	// The explicit-start-bound twin of the re-slice fix: `self.data[1:self.len]`
	// must offset the base slice's own .data pointer by the runtime start, not
	// decay a raw array — proving the offset math, not just the zero-start
	// case. arr = [10,20,30,40], self.data[1:4] = [20,30,40], so s[1] = 30 is
	// the exit code.
	emitAndRun(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[1:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 4 };
    var s []int = view(b);
    return s[1];
}`, false, 30, false)
}

func TestEmitReSliceSliceFieldWritesC(t *testing.T) {
	// The emitted C for the re-slice fixture: the helper's CheckedSlice over a
	// Load(FieldPlace) slice base emits the same two-statement shape as the
	// array base (a temp declaration holding the checked-start result, then
	// the compound-literal construction using that temp for both .data and
	// .len), but with the slice-specific pieces: the runtime helper's upper
	// bound argument is the base slice's RUNTIME .len field
	// (pebble_local_28.pebble_field_25.len), the construction's .data is the
	// base slice's OWN .data pointer offset by the temp
	// (pebble_local_28.pebble_field_25.data + pebble_slice_ret_24), and the
	// .len is the runtime end bound minus the temp. Symbols 24 (slice type),
	// 25 (data field), 26 (len field), 28 (self), return value node 24, and
	// field value node 34 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, `type Bag = struct {
    data []int;
    len uint;
};
fn view(self Bag) []int {
    return self.data[:self.len];
}
fn main() int {
    var arr [4]int = [10, 20, 30, 40];
    var b Bag = Bag.{ data = arr[:], len = 3 };
    var s []int = view(b);
    return s[2];
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_ret_24 = pebble_rt_checked_slice_start_i32(0, pebble_local_28.pebble_field_26, pebble_local_28.pebble_field_25.len, (PebbleSourceLoc){\"main.peb\"",
		"return (pebble_slice_24_t){ .data = pebble_local_28.pebble_field_25.data + pebble_slice_ret_24, .len = (size_t)(pebble_local_28.pebble_field_26 - pebble_slice_ret_24) };",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 30, false)
}

func TestEmitPrintIndexesMethodCallSliceResultCompilesAndRuns(t *testing.T) {
	// The confirmed real-world blocking shape: indexing a MethodCall's slice
	// result directly inside a print statement (`print b.view()[1];`,
	// examples/read_file.peb's original contents.as_slice()[i] pattern before
	// it was rewritten to a workaround). The base (b.view()) has no
	// addressable place, so the checker lowers this to a bare CheckedIndex;
	// buildPrint threads buildSliceIndexValue's leading temp-declaration
	// statement into the print statement's own sequence. Captured stdout
	// asserts the printed value is the correct element (20, index 1 of
	// [10,20,30]) - not just that Emit succeeded.
	output := emitAndRunCapture(t, `type Bag = struct {
    data []i32;
    fn view(self Bag) []i32 { return self.data[:]; }
};
fn main() int {
    var a [3]i32 = [10, 20, 30];
    var b Bag = Bag.{ data = a[:] };
    print b.view()[1];
    return 0;
}`, false, 0, false)
	if output != "20\n" {
		t.Fatalf("captured output = %q, want %q", output, "20\n")
	}
}

func TestEmitPrintIndexesMethodCallSliceResultEvaluatesBaseOnce(t *testing.T) {
	// Correctness-critical: the base method call must run EXACTLY ONCE, not
	// once for the bounds-check .len and again for the .data read - a naive
	// lowering that referenced the call expression twice would run the
	// call's side effect (here, a print) twice. The helper prints 99 as its
	// own side effect before returning the slice; capturing stdout and
	// counting occurrences of "99" proves single evaluation.
	output := emitAndRunCapture(t, `type Bag = struct {
    data []i32;
    fn view(self Bag) []i32 { print 99; return self.data[:]; }
};
fn main() int {
    var a [3]i32 = [10, 20, 30];
    var b Bag = Bag.{ data = a[:] };
    print b.view()[1];
    return 0;
}`, false, 0, false)
	if want := "99\n20\n"; output != want {
		t.Fatalf("captured output = %q, want %q (base call must be evaluated exactly once)", output, want)
	}
}

func TestEmitPrintIndexesMethodCallSliceResultCharElementCompilesAndRuns(t *testing.T) {
	// The char-element twin: indexing a MethodCall's []char result directly
	// inside print - the exact type read_file.peb's contents.as_slice()[i]
	// shape needed. Routed through buildCharOperand's CheckedIndex case
	// (distinct from the i32 case's buildExpr path) via the same
	// buildSliceIndexValue helper.
	output := emitAndRunCapture(t, `type Box = struct {
    data []char;
    fn view(self Box) []char { return self.data[:]; }
};
fn main() int {
    var a [3]char = ['h', 'i', '!'];
    var b Box = Box.{ data = a[:] };
    print b.view()[1];
    return 0;
}`, false, 0, false)
	if output != "i\n" {
		t.Fatalf("captured output = %q, want %q", output, "i\n")
	}
}

func TestEmitIndexesSliceTypedFieldDirectlyCompilesAndRuns(t *testing.T) {
	// The "cheap to duplicate" base shape: indexing a slice-typed struct
	// field directly (self.data[i], a Load of a slice-typed place) needs no
	// temp - it's a pure, side-effect-free projection, safe to reference
	// twice (once for the bounds-check .len, once for .data). This must keep
	// working exactly as before this fix (it was already reachable via
	// Load(CheckedIndexPlace) for an addressable receiver).
	emitAndRun(t, `type Bag = struct {
    data []i32;
    fn peek(self Bag) i32 { return self.data[1]; }
};
fn main() int {
    var a [3]i32 = [10, 20, 30];
    var b Bag = Bag.{ data = a[:] };
    return b.peek();
}`, false, 20, false)
}

func TestEmitSliceStructFieldInlineConstructionCompilesAndRuns(t *testing.T) {
	// The exact repro this gap was filed for: a plain, non-generic struct with
	// a slice-typed field whose construction value is an inline slice
	// expression (`Bag.{ items = arr[:] }`). The RecordConstruct's field value
	// is a bare CheckedSlice node (confirmed against a real fixture dump), so
	// the construction needs the same two-statement temp-then-construction
	// shape a slice local's declaration and a slice return use: the checked
	// slice-start call is hoisted into a temp statement threaded ahead of the
	// struct declaration line, and the slice compound literal uses that temp
	// for both its .data offset and its .len. arr[:] = [1,2,3], so b.items[1] =
	// 2 is the exit code.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = arr[:] };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitSliceStructFieldInlineConstructionEmitsTempStatement(t *testing.T) {
	// The emitted C for the inline-construction fixture: the struct field
	// construction is the two-statement shape — a pebble_field_slice_<nodeID>
	// temp declaration holding the checked slice-start result, then the struct
	// declaration whose slice field's compound literal uses that temp for both
	// .data and .len. The temp name derives from the field value node's NodeID
	// (symbols 24 (Bag), 25 (items), 27 (arr), 28 (b), field value node 17, and
	// slice type 24 come from the real fixture dump), distinct from the
	// pebble_slice_start_<symbol> and pebble_slice_ret_<nodeID> temps so the
	// three can never collide.
	unit, snapshot, entryID, sources := buildFixture(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag = Bag.{ items = arr[:] };
    return b.items[1];
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t *data;\n    size_t len;\n} pebble_slice_24_t;",
		"pebble_slice_24_t pebble_field_25;",
		"int32_t pebble_field_slice_17 = pebble_rt_checked_slice_start_i32(0, 3, 3, (PebbleSourceLoc){\"main.peb\"",
		"pebble_struct_23_t pebble_local_30 = { .pebble_field_25 = (pebble_slice_24_t){ .data = pebble_local_29 + pebble_field_slice_17, .len = (size_t)(3 - pebble_field_slice_17) } };",
		"return pebble_local_30.pebble_field_25.data[pebble_rt_checked_index_i32(1, (int32_t)pebble_local_30.pebble_field_25.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceStructFieldSliceFromRawCompilesAndRuns(t *testing.T) {
	// The same gap via SliceFromRaw: a raw-pointer-derived slice (`slice ptr,
	// n` — the raw-slice builtin, restricted to std-package source) used
	// directly as a slice-typed field's construction value. The RecordConstruct
	// field value is a bare SliceFromRaw node (confirmed against a real fixture
	// dump), whose construction is a single expression with no temp
	// (buildRawSliceConstruction emits the compound literal directly). The
	// 1-element slice over the 42 value means b.items[0] = 42 is the exit code.
	unit, snapshot, entryID, sources := buildStdFixture(t, `type Bag = struct { items []i32; };
fn main() i32 {
    var value i32 = 42;
    var ptr *i32 = &value;
    var b Bag = Bag.{ items = slice ptr, 1 };
    return b.items[0];
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The SliceFromRaw construction is a single expression: no temp statement,
	// and the field value is the compound literal directly.
	if strings.Contains(out, "pebble_field_slice_") {
		t.Errorf("emitted C unexpectedly contains a field temp statement:\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_25 = (pebble_slice_24_t){ .data = pebble_local_30, .len = (size_t)(1) }") {
		t.Errorf("emitted C missing the inline SliceFromRaw field construction:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitSliceFieldReassignmentFromRawCompilesAndRuns(t *testing.T) {
	// A bare SliceFromRaw as a slice-field REASSIGNMENT value — the
	// std/string.peb grow shape (`self.data = slice ptr, new_cap;`), where the
	// Store's value is a SliceFromRaw node rather than a reference to a
	// slice-typed local. Before the fix, buildStoreCore's isSlice branch
	// rejected anything but a SymbolValue with "reassigns a slice-typed place
	// from a SliceFromRaw, want a reference to a slice-typed local in scope".
	// The 1-element slice over the 42 value means b.items[0] = 42 is the exit
	// code, proving the whole SliceFromRaw reassignment works at runtime, not
	// just that it emits.
	unit, snapshot, entryID, sources := buildStdFixture(t, `type Bag = struct { items []i32; };
fn main() i32 {
    var value i32 = 42;
    var ptr *i32 = &value;
    var b Bag = Bag.{ items = slice ptr, 1 };
    b.items = slice ptr, 1;
    return b.items[0];
}`, "main")
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 42, false)
}

func TestEmitSliceStructFieldLocalReferenceCompilesAndRuns(t *testing.T) {
	// The already-working shape must keep working exactly as before: construct
	// the slice into a local FIRST, then use that local as the field's
	// construction value. The field value is a SymbolValue naming a slice-typed
	// local, emitted as the local's own pebble_local_<symbol> C name with no
	// temp statement. arr[:] = [1,2,3], so b.items[1] = 2 is the exit code.
	emitAndRun(t, `type Bag = struct { items []int; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var s []int = arr[:];
    var b Bag = Bag.{ items = s };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitGenericSliceStructFieldInlineConstructionCompilesAndRuns(t *testing.T) {
	// The generic case this gap was originally investigated for: a generic
	// struct with a slice field instantiated as Bag[int] and constructed inline
	// (`Bag[int].{ items = arr[:] }`). The root cause is unrelated to
	// genericity — the field value is the same bare CheckedSlice — so this
	// general fix must also resolve the generic case. arr[:] = [1,2,3], so
	// b.items[1] = 2 is the exit code.
	emitAndRun(t, `type Bag[K] = struct { items []K; };
fn main() int {
    var arr [3]int = [1, 2, 3];
    var b Bag[int] = Bag[int].{ items = arr[:] };
    return b.items[1];
}`, false, 2, false)
}

func TestEmitOptionalResultCompilesAndRuns(t *testing.T) {
	// The exact repro this slice was filed for: an optional-returning helper
	// called as the direct initializer of a matching optional-typed local.
	// `return 5;` is an implicit injection whose return child is the bare
	// payload value (confirmed against a real fixture dump), so the backend
	// supplies the injection itself; has_value must be true and the unwrapped
	// value must be 5.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultNoneCompilesAndRuns(t *testing.T) {
	// `return none;` from an optional-result helper: the caller-side has_value
	// must be false, so the false path of the if is taken and 0 is returned.
	// Both the bare tail return and the `some`-explicit form are exercised.
	emitAndRun(t, "fn f() ?int { return none; } fn main() int { var o ?int = f(); if o.has_value { return 99; } return 0; }", false, 0, false)
	emitAndRun(t, "fn f() ?int { return some 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultImplicitInjectionCompilesAndRuns(t *testing.T) {
	// The implicit-injection-in-return path specifically: `return 5;` with no
	// `some` keyword. The checker emits the bare payload IntegerLiteral as the
	// return child (no OptionalInject wrapper, unlike a local declaration's
	// OptionalInject and unlike an aggregate payload's OptionalInject), so the
	// backend's buildOptionalReturnValue must supply the has_value-true
	// injection itself. The unwrapped value round-trips to 5.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
	// The explicit `some` form is the same C, confirming both spellings lower
	// identically through the return path.
	emitAndRun(t, "fn f() ?int { return some 5; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultForwardsLocalCompilesAndRuns(t *testing.T) {
	// A SymbolValue return: the helper declares an optional-typed local (itself
	// implicitly injected from the bare integer 5, exercising the existing
	// OptionalInject local-declaration path) and `return o;` forwards it,
	// emitting `return pebble_local_<o>;`. The entry assigns the call to a
	// matching optional local and unwraps 5.
	emitAndRun(t, "fn f() ?int { let o ?int = 5; return o; } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultCallsHelperCompilesAndRuns(t *testing.T) {
	// One optional-returning helper calling another (`return g();`): the
	// return child is a DirectCall carrying the optional result type, built by
	// the same buildDirectCall machinery any call uses and forwarded as the
	// return value — the call already returns the optional's own C type.
	// g's 5 flows through f to the entry's unwrap.
	emitAndRun(t, "fn g() ?int { return 5; } fn f() ?int { return g(); } fn main() int { var o ?int = f(); if o.has_value { return o!; } return 0; }", false, 5, false)
}

func TestEmitOptionalResultBoolPayloadCompilesAndRuns(t *testing.T) {
	// A bool-payload optional result: the payload is built by buildBoolExpr
	// (not hardcoded to an integer payload), and the unwrapped bool drives an
	// if at the call site (`if o!` — the same shape the existing bool optional
	// tests use for a local). The some-true and none forms both round-trip.
	emitAndRun(t, "fn f() ?bool { return some true; } fn main() int { var o ?bool = f(); if o! { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?bool { return none; } fn main() int { var o ?bool = f(); if !o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultStructPayloadCompilesAndRuns(t *testing.T) {
	// A struct-payload optional result: `return some P.{ x = 1, y = 2 };` is a
	// SomeOptional wrapping a RecordConstruct, built by the shared
	// buildOptionalValueExpr → buildStructValueExpr dispatch (the same payload
	// grammar a nested-in-aggregate optional uses). has_value must be true at
	// the call site and the emitted optional's value field must hold the
	// constructed struct. The payload field read-back (o!.x) is separately out
	// of scope — the caller-side FieldValue-on-unwrap read is a pre-existing,
	// unrelated limitation — so the value round-trip is asserted on the emitted
	// C (see TestEmitOptionalResultStructPayloadWritesC), which would fail to
	// compile under -Wall -Wextra -Werror if the payload type dispatch were
	// wrong. (A `return none;` variant for an aggregate payload is not covered
	// here: it trips a pre-existing, unrelated isEnumType heuristic that
	// misclassifies a struct payload as an enum when the struct is never
	// constructed anywhere in the program — reproducible with a bare
	// `var o ?P = none;` in main, no helper functions involved.)
	emitAndRun(t, "type P = struct { x int; y int; };\nfn f() ?P { return some P.{ x = 1, y = 2 }; } fn main() int { var o ?P = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultTuplePayloadCompilesAndRuns(t *testing.T) {
	// A tuple-payload optional result, in both spellings: the explicit
	// `return some (1, 2);` (SomeOptional wrapping a TupleValue) and the
	// implicit `return (1, 2);` whose aggregate payload the checker wraps in an
	// OptionalInject node (confirmed against a real fixture dump) — the shape
	// that motivated extending buildOptionalValueExpr to share the SomeOptional
	// case. has_value must be true at the call site; the tuple element
	// read-back (o!.0) is separately out of scope, so the value is asserted on
	// the emitted C, which would fail to compile if the payload dispatch were
	// wrong.
	emitAndRun(t, "fn f() ?(int, int) { return some (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
	emitAndRun(t, "fn f() ?(int, int) { return (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitOptionalResultWritesC(t *testing.T) {
	// The emitted C for the flagship repro: the optional typedef (bool
	// has_value plus the payload's int32_t value) precedes the helper, the
	// helper's signature declares its return type as pebble_optional_23_t (the
	// optional's own typedef, not the entry's scalar int32_t), its return
	// statement emits the injected compound literal
	// (pebble_optional_23_t){ .has_value = true, .value = 5 } for the bare
	// payload `return 5;`, and the call site initializes the local directly
	// from pebble_fn_24(ctx). Symbols 24 (f), 25 (main), 26 (o local), and
	// optional type 23 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    int32_t value;\n} pebble_optional_23_t;",
		"static pebble_optional_23_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_optional_23_t){ .has_value = true, .value = 5 };",
		"pebble_optional_23_t pebble_local_28 = pebble_fn_24(ctx);",
		"    (void)pebble_local_28;",
		"if (pebble_local_28.has_value) {",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	typedefIndex := strings.Index(out, "typedef struct")
	helperIndex := strings.Index(out, "static pebble_optional_23_t pebble_fn_24")
	if typedefIndex < 0 || helperIndex < 0 || typedefIndex > helperIndex {
		t.Errorf("optional typedef does not precede the helper function (definition before use):\n%s", out)
	}
}

func TestEmitOptionalResultStructPayloadWritesC(t *testing.T) {
	// The emitted C for the struct-payload fixture: the struct typedef
	// precedes the optional typedef that names it as its value field
	// (definition before use — the same ordering the aggregate-typedef DFS
	// guarantees for tuple/struct payloads), and the helper's return emits the
	// nested compound literal with the struct construction as .value. Symbols
	// 24 (P), 25 (x), 26 (y), 27 (f), 28 (main), 29 (o local), struct type 23,
	// and optional type 24 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "type P = struct { x int; y int; };\nfn f() ?P { return some P.{ x = 1, y = 2 }; } fn main() int { var o ?P = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    bool has_value;\n    pebble_struct_23_t value;\n} pebble_optional_24_t;",
		"static pebble_optional_24_t pebble_fn_27(PebbleContext *ctx) {",
		"    return (pebble_optional_24_t){ .has_value = true, .value = (pebble_struct_23_t){ .pebble_field_25 = 1, .pebble_field_26 = 2 } };",
		"pebble_optional_24_t pebble_local_31 = pebble_fn_27(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	structIndex := strings.Index(out, "typedef struct {\n    int32_t pebble_field_25")
	optionalIndex := strings.Index(out, "} pebble_optional_24_t;")
	if structIndex < 0 || optionalIndex < 0 || structIndex > optionalIndex {
		t.Errorf("struct typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalResultTuplePayloadWritesC(t *testing.T) {
	// The emitted C for the tuple-payload implicit-injection fixture: the
	// tuple typedef precedes the optional typedef, and the helper's return
	// emits the nested compound literal with the tuple construction as .value —
	// the OptionalInject-in-return shape, sharing buildOptionalValueExpr's
	// SomeOptional case. Symbols 24 (f), 25 (main), 26 (o local), tuple type
	// 23, and optional type 24 come from the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn f() ?(int, int) { return (1, 2); } fn main() int { var o ?(int, int) = f(); if o.has_value { return 1; } return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"typedef struct {\n    int32_t _0;\n    int32_t _1;\n} pebble_tuple_23_t;",
		"typedef struct {\n    bool has_value;\n    pebble_tuple_23_t value;\n} pebble_optional_24_t;",
		"static pebble_optional_24_t pebble_fn_24(PebbleContext *ctx) {",
		"    return (pebble_optional_24_t){ .has_value = true, .value = (pebble_tuple_23_t){ 1, 2 } };",
		"pebble_optional_24_t pebble_local_28 = pebble_fn_24(ctx);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	tupleIndex := strings.Index(out, "} pebble_tuple_23_t;")
	optionalIndex := strings.Index(out, "} pebble_optional_24_t;")
	if tupleIndex < 0 || optionalIndex < 0 || tupleIndex > optionalIndex {
		t.Errorf("tuple typedef does not precede the optional typedef that names it (definition before use):\n%s", out)
	}
}

func TestEmitOptionalParameterCompilesAndRuns(t *testing.T) {
	// Optional-typed PARAMETERS: a helper taking a ?int parameter, called
	// with a scalar implicit-injection argument (`g(5)`, which arrives as an
	// OptionalInject node at a call site — unlike a return position's bare
	// payload). The parameter is seeded into the callee's scope exactly like
	// an optional local (localInfo{optional: ...}), so a body read
	// (o.has_value) resolves through the existing optional-local machinery.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(5); }", false, 1, false)
}

func TestEmitOptionalParameterNoneArgumentCompilesAndRuns(t *testing.T) {
	// A fresh `none` passed directly as the argument.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(none); }", false, 0, false)
}

func TestEmitOptionalParameterSomeArgumentCompilesAndRuns(t *testing.T) {
	// A fresh `some x` passed directly as the argument.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some 5); }", false, 1, false)
}

func TestEmitOptionalParameterForwardsLocalCompilesAndRuns(t *testing.T) {
	// An already-declared optional-typed local passed as the argument — the
	// SymbolValue-forward shape.
	emitAndRun(t, "fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { var n ?int = none; return g(n); }", false, 0, false)
}

func TestEmitOptionalParameterForwardsCallResultCompilesAndRuns(t *testing.T) {
	// The result of another optional-returning call passed directly as the
	// argument — the DirectCall-forward shape.
	emitAndRun(t, "fn f() ?int { return 5; } fn g(o ?int) int { if o.has_value { return 1; } return 0; } fn main() int { return g(f()); }", false, 1, false)
}

func TestEmitOptionalParameterStructPayloadCompilesAndRuns(t *testing.T) {
	// A tuple/struct-payload optional parameter, not just an integer payload
	// — confirms the payload-type dispatch isn't hardcoded to scalars. Bool
	// implicit injection is checker-rejected (T0505, matching the identical
	// limitation on the return-position work), so struct/tuple payloads use
	// explicit `some`.
	emitAndRun(t, "type P = struct { x int; y int; };\nfn g(o ?P) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some P.{ x = 1, y = 2 }); }", false, 1, false)
}

func TestEmitOptionalParameterTuplePayloadCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "fn g(o ?(int, int)) int { if o.has_value { return 1; } return 0; } fn main() int { return g(some (1, 2)); }", false, 1, false)
}

func TestEmitOptionalResultTestsStillPass(t *testing.T) {
	// Regression guard: the optional-RESULT machinery (landed earlier this
	// session) is unaffected by adding parameter support — the shared
	// buildOptionalValue (generalized from buildOptionalReturnValue to serve
	// both the return and call-argument positions) still returns the exact
	// same C for a return-position optional value it did before
	// generalization.
	emitAndRun(t, "fn f() ?int { return 5; } fn main() int { var o ?int = f(); if o.has_value { return 1; } return 0; }", false, 1, false)
}

func TestEmitNoneOptionalOfConstructedStructCompilesAndRuns(t *testing.T) {
	// Found during a real-code audit: isEnumType (the function distinguishing
	// a Nominal type's struct-vs-enum classification) used to guess from
	// FieldPlace/RecordConstruct usage evidence and defaulted to "enum" when
	// it found none — wrong for `var o ?P = none;`, since a `none` literal
	// never actually constructs its payload type, so the struct P was
	// misclassified as an enum and the optional's .value field referenced an
	// enum typedef that was never emitted. Fixed by reading each member's
	// own declaration-node kind directly (tir.FieldDeclaration vs
	// tir.VariantDeclaration, unconditional evidence, not a usage-evidence
	// guess). This also required two supporting fixes: collectStructTypes
	// now also collects a struct type reached only via an optional's payload
	// (mirroring the existing Parameters/ResultType scans), and a
	// NoneOptional's irrelevant .value field now uses the correct C
	// zero-initializer shape for its payload type ({0} for a struct/tuple,
	// which a bare 0 doesn't satisfy under -Wmissing-field-initializers /
	// -Wmissing-braces with -Werror; 0 for a scalar/enum payload, unchanged).
	// P here has real field evidence elsewhere in the program (constructed
	// by make()) — the deeper, separate case of a struct with ZERO field
	// evidence anywhere (declared, never constructed, ONLY ever named as a
	// none optional's payload) is still blocked by a different, narrower gap
	// (field-type resolution has no fallback when no FieldPlace/
	// RecordConstruct evidence exists anywhere in the whole program) —
	// tracked separately, not fixed here.
	emitAndRun(t, "type P = struct { x int; y int; };\nfn make() P { return P.{ x = 1, y = 2 }; }\nfn main() int {\nvar p P = make();\nvar o ?P = none;\nif o.has_value { return 1; }\nreturn p.x;\n}", false, 1, false)
}

func TestEmitNoneOptionalOfUnusedStructCompilesAndRuns(t *testing.T) {
	// P is only named as the payload of an absent optional. Its fields have no
	// FieldPlace or RecordConstruct usage evidence, so this exercises the
	// declaration-level member type carried by TypeDecl.
	emitAndRun(t, "type P = struct { x int; y int; }; fn main() int { var o ?P = none; if o.has_value { return 1; } return 0; }", false, 0, false)
}

func TestEmitSliceParameterWritesC(t *testing.T) {
	// The parameter C type for a slice-taking helper: the C signature declares
	// the parameter as the slice type's own struct typedef (the same
	// pebble_slice_<typeID>_t 10.37's local declaration builds, no new typedef
	// shape) with the pebble_local_<symbol> naming every parameter uses, plus
	// the (void) cast every parameter gets, and the call site passes the
	// slice-typed local's own C name. Symbols 24 (first), 25 (its s parameter),
	// 28 (the entry's s local), and slice type 23 come from the real fixture
	// dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn first(s []i32) i32 { return s[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; return first(s); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"static int32_t pebble_fn_24(PebbleContext *ctx, pebble_slice_23_t pebble_local_25) {",
		"    (void)pebble_local_25;",
		"return pebble_local_25.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_25.len, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_fn_24(ctx, pebble_local_30);",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 2, false)
}

func TestEmitSliceConstructionAsCallArgumentCompilesAndRuns(t *testing.T) {
	// The leading-statement-argument slice: an inline slice construction passed
	// directly as a call argument works when the CALL ITSELF is in a
	// leading-statement position, because that position has a natural place for
	// the temp-declaration statement the construction needs (the same pre-
	// threading buildScalarInitializeCore demonstrates). This is the exact shape
	// examples/prime_sieve.peb needs (sieve(primes[:], limit)) — previously
	// worked around there by binding the slice to a local first. The fixture
	// exercises BOTH supported positions: a bare void call statement
	// (mark(b[2:4])) and a scalar local's declaration initializer
	// (let r = first(a[1:3])). a[1] is 20, so the exit code 20 (returned only
	// when the mark() call actually set b[2] through the bool slice) proves
	// both slices were constructed and passed correctly at runtime, not just
	// emitted.
	emitAndRun(t, `fn first(x []int) int { return x[0]; }
fn mark(x []bool) void { x[0] = true; }
fn main() int {
    var a [5]int = [10, 20, 30, 40, 50];
    var b [5]bool = [false; 5];
    mark(b[2:4]);
    let r = first(a[1:3]);
    if b[2] {
        return r;
    } else {
        return 0;
    }
}`, false, 20, false)
}

func TestEmitSliceConstructionAsCallArgumentEmitsTempStatement(t *testing.T) {
	// The emitted C for the inline-construction call statement: the slice
	// argument is the two-statement temp-then-construction shape a slice local's
	// declaration and the return side already use — a pebble_slice_arg_<nodeID>
	// temp declaration holding the checked slice-start result, emitted as a
	// statement line directly before the call, then the call whose argument is
	// the slice compound literal using that temp for both .data and .len.
	// b[2:4] over a 5-element bool array: checked start (2, 4, 5), .data offset
	// 2, .len 2.
	unit, snapshot, entryID, sources := buildFixture(t, `fn mark(x []bool) void { x[0] = true; }
fn main() int {
    var b [5]bool = [false; 5];
    mark(b[2:4]);
    return 0;
}`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_arg_",
		"pebble_rt_checked_slice_start_i32(2, 4, 5, (PebbleSourceLoc){\"main.peb\"",
		"pebble_local_",
		".len = (size_t)(4 - pebble_slice_arg_",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	if !strings.Contains(out, ".data = pebble_local_") {
		t.Errorf("emitted C missing slice compound literal .data offset:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitArrayElementWriteCompilesAndRuns(t *testing.T) {
	// The flagship array element write: a[2] = 99 replaces the middle slot of
	// [1,2,3,4,5], and the sum of all five slots read back (1 + 2 + 99 + 4 + 5
	// = 111) confirms the write landed at exactly the indexed slot and
	// clobbered nothing else — not just that the program compiled.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; a[2] = 99; return a[0] + a[1] + a[2] + a[3] + a[4]; }", false, 111, false)
}

func TestEmitSliceElementWriteCompilesAndRuns(t *testing.T) {
	// The flagship slice element write: s = a[1:3] = [2, 3], s[0] = 9 replaces
	// the first slot of the slice's view, and s[0] + s[1] = 9 + 3 = 12 read
	// back confirms the write through the slice actually changed the underlying
	// element the slice views.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; s[0] = 9; return s[0] + s[1]; }", false, 12, false)
}

func TestEmitSliceParameterElementWriteObservedByCaller(t *testing.T) {
	// A slice-typed parameter's element written inside the helper, observed by
	// the caller after the call returns: set writes s[0] = 9 where the caller
	// passed s = a[1:3], so the write lands in a[1] (the slice's first slot is
	// the caller's array's second slot — a slice is a non-owning view). The
	// caller then reads its OWN array local a[1] and sees 9. This is the real
	// proof the write machinery is correct — the mutation reached the same
	// backing array the caller owns, not just some helper-local copy.
	emitAndRun(t, "fn set(s []i32) void { s[0] = 9; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; set(s); return a[1]; }", false, 9, false)
}

func TestEmitArrayBoolElementWriteCompilesAndRuns(t *testing.T) {
	// A bool-element array write: a[0] = true replaces a false slot, and the
	// element read back drives an if condition — exit 1 proves the bool write
	// landed and read back correctly (not just that it compiled).
	emitAndRun(t, "fn main() i32 { var a [2]bool = [false, false]; a[0] = true; if a[0] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitSliceBoolElementWriteCompilesAndRuns(t *testing.T) {
	// A bool-element slice write: s = a[1:3] = [false, true], s[0] = true
	// replaces the view's first (false) slot, and reading s[0] back drives an
	// if condition — exit 1 proves the bool write through the slice landed.
	emitAndRun(t, "fn main() i32 { var a [4]bool = [true, false, true, false]; var s []bool = a[1:3]; s[0] = true; if s[0] { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitI64ArrayElementWriteCompilesAndRuns(t *testing.T) {
	// The width-generic path holds for writes too: an i64 entry writes an i64
	// array element (the lvalue lowers through pebble_rt_checked_index_i64 and
	// the RHS through buildExpr at i64), and a[1] = 21 read back is the exit
	// code.
	emitAndRun(t, "fn main() i64 { var a [2]i64 = [20, 22]; a[1] = 21; return a[1]; }", false, 21, false)
}

func TestEmitArrayElementWriteOutOfBoundsAborts(t *testing.T) {
	// An out-of-bounds array element WRITE: a[i] = 9 with a runtime i = 5 on a
	// [2]i32 array must panic through the exact same pebble_rt_checked_index_i32
	// call the read side uses — the lvalue text is identical either way, so
	// the write's bounds check fires at runtime (the runtime index bypasses the
	// checker's compile-time validation), not just a compile-time rejection.
	emitAndRun(t, "fn main() i32 { var a [2]i32 = [10, 20]; var i i32 = 5; a[i] = 9; return a[0]; }", false, 0, true)
}

func TestEmitSliceElementWriteOutOfBoundsAborts(t *testing.T) {
	// An out-of-bounds slice element WRITE: s = a[1:3] has len 2, and s[i] = 9
	// with a runtime i = 5 must panic through
	// pebble_rt_checked_index_i32(i, (int32_t)s.len) at runtime.
	emitAndRun(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; var i i32 = 5; s[i] = 9; return s[0]; }", false, 0, true)
}

func TestEmitArrayElementWriteWritesC(t *testing.T) {
	// The emitted C for an array element write: the Store lowers to a plain
	// assignment expression whose lvalue is the exact bounds-checked subscript
	// buildPlaceLValue's CheckedIndexPlace case produces for an array base —
	// pebble_local_27[pebble_rt_checked_index_i32(0, 5)] = 9; — with no new
	// bounds-check call site. Symbols 27 (the array local a) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; a[0] = 9; return a[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_local_27[5] = { 1, 2, 3, 4, 5 };",
		"pebble_local_27[pebble_rt_checked_index_i32(0, 5, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_local_27[pebble_rt_checked_index_i32(0, 5, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 9, false)
}

func TestEmitSliceElementWriteWritesC(t *testing.T) {
	// The emitted C for a slice element write: the Store lowers to a plain
	// assignment expression whose lvalue is the exact bounds-checked .data
	// subscript buildPlaceLValue's CheckedIndexPlace case produces for a slice
	// base — pebble_local_28.data[pebble_rt_checked_index_i32(0,
	// (int32_t)pebble_local_28.len)] = 9; — the .len bound checked against the
	// slice's own length. Symbols 27 (array a), 28 (slice s) come from the real
	// fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; var s []i32 = a[1:3]; s[0] = 9; return s[0]; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"int32_t pebble_slice_start_28 = pebble_rt_checked_slice_start_i32(1, 3, 5, (PebbleSourceLoc){\"main.peb\"",
		"pebble_local_28.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_28.len, (PebbleSourceLoc){\"main.peb\"",
		"return pebble_local_28.data[pebble_rt_checked_index_i32(0, (int32_t)pebble_local_28.len, (PebbleSourceLoc){\"main.peb\"",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
	compileAndRun(t, buf.Bytes(), 9, false)
}

func TestEmitPointerReceiverSliceIndexAddressOfCompilesAndRuns(t *testing.T) {
	// The returned pointer must alias the backing slice in the original struct:
	// get(1) returns &self.data[1], and the caller mutates that element through
	// the returned pointer before reading it back through the struct field.
	source := "type V = struct { data []i32; fn get(self *V, index i32) *i32 { return &self.data[index]; } }; fn main() i32 { var values [3]i32 = [1, 2, 3]; let data []i32 = values[:]; var v V = V.{ data = data }; let pointer *V = &v; let p *i32 = pointer.get(1); *p = 9; return v.data[1]; }"
	emitAndRun(t, source, false, 9, false)
}

func TestEmitStructPointerRoundTripCompilesAndRuns(t *testing.T) {
	t.Skip("blocked: (*p).x on a struct pointer degrades to a tir.FieldValue node (field-of-value, not field-of-place) because the checker's place-tracking doesn't extend a DereferencePlace through a field-access base in this position — confirmed the same gap blocks even materializing the whole dereferenced struct into a local (`let v Point = *p;` also fails). Needs new struct-rvalue backend support, scoped as its own follow-up in spec/compiler/proposals/11-raw-pointers-and-unsafe-ops.md.")
	// Takes the address of a struct local, dereferences the pointer, and reads
	// a field through the dereferenced result.
	// type Point = struct { x i32; y i32; }
	// fn main() i32 { var point Point = Point.{ x = 3, y = 4 }; let p *Point = &point; return (*p).x; }
	emitAndRun(t, "type Point = struct { x i32; y i32; }; fn main() i32 { var point Point = Point.{ x = 3, y = 4 }; let p *Point = &point; return (*p).x; }", false, 3, false)
}

func TestEmitVariadicCallSumsCollectedSliceCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: fn sum(...values []int) int { return
	// values[0] + values[1] + values[2]; } fn main() int { return sum(10,
	// 20, 30); } Confirms the collected variadic slice has both the right
	// length and the right values, by actually summing them (not just
	// reading .len), proving buildVariadicSliceArgument's compound-literal
	// array is populated correctly, not merely present.
	emitAndRun(t, "fn sum(...values []int) int { return values[0] + values[1] + values[2]; } fn main() int { return sum(10, 20, 30); }", false, 60, false)
}

func TestEmitFunctionTypedLocalDoesNotRegressAllocator(t *testing.T) {
	// The general indirect-call path (buildFunctionIndirectCall) must not
	// interfere with the pre-existing allocator-specific indirect call
	// (context.default_allocator.alloc(...) and friends), which shares the
	// same tir.IndirectCall node kind but is detected and handled by a wholly
	// separate branch (allocatorCallee). Regression-covered already by this
	// file's existing TestEmitRuntimeAllocator* tests (run as part of the
	// full suite); this test adds one more real allocation round trip
	// alongside a function-typed local in the same program, confirming the
	// two mechanisms coexist correctly.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int {\nvar f fn(int, int) int = add;\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = f(3, 4);\nreturn *p;\n}", false, 7, false)
}

func TestEmitFunctionTypedStructFieldCallCompilesAndRuns(t *testing.T) {
	// The exact minimal repro: constructing a struct with a function-typed
	// field (Table.{ op = add }) and calling directly through the field
	// (t.op(1, 2)), no intermediate local. The field read reaches
	// buildFunctionValue as a bare FieldValue (the indirect call's direct
	// -callee shape), distinct from the Load(FieldPlace)-wrapped shape a
	// non-callee position uses (see the next test).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return t.op(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedStructFieldViaLocalCompilesAndRuns(t *testing.T) {
	// Reading a function-typed field into a local first, then calling
	// through the local — confirms the field-read value forwards correctly
	// into buildFunctionLocalDeclaration (slice 1) unchanged, and exercises
	// the Load(FieldPlace)-wrapped field-read shape a local-declaration
	// initializer position produces (distinct from the direct-callee
	// FieldValue shape the previous test uses).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; var f fn(int, int) int = t.op; return f(1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedStructFieldNeverReadCompilesAndRuns(t *testing.T) {
	// A function-typed field that is constructed but never read back by
	// name anywhere else — confirms the typedef-collection and
	// reachability-walk fixes (RecordConstruct.Fields isn't part of
	// node.Children, so both walks need an explicit case to find a
	// HoistedFunctionValue used only as a field's construction value)
	// correctly discover the referenced function and its typedef even
	// though nothing calls through the field at all.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return 5; }", false, 5, false)
}

func TestEmitFunctionTypedStructFieldWritesC(t *testing.T) {
	// Confirm the emitted C directly: the function typedef appears BEFORE
	// the struct typedef that names it as a field's C type (slice 2
	// reverses slice 1's "function typedefs are self-contained, append
	// last" assumption, since a struct field can now reference a function
	// typedef), and the field value is assigned bare (no cast).
	unit, snapshot, entryID, sources := buildFixture(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int { var t Table = Table.{ op = add }; return t.op(1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	fnptrIndex := strings.Index(out, "typedef int32_t (*pebble_fnptr_")
	structIndex := strings.Index(out, "typedef struct {\n    pebble_fnptr_")
	if fnptrIndex < 0 || structIndex < 0 || fnptrIndex > structIndex {
		t.Errorf("function typedef does not precede the struct typedef that names it as a field (definition before use):\n%s", out)
	}
	if !strings.Contains(out, ".pebble_field_") || !strings.Contains(out, "= pebble_fn_") {
		t.Errorf("emitted C missing expected field-initializer shape:\n%s", out)
	}
}

func TestEmitFunctionTypedStructFieldDoesNotRegressAllocator(t *testing.T) {
	// The exact collision this slice was built to avoid: a function-typed
	// struct field read (t.op) produces the same FieldValue TIR node kind a
	// real allocator field access does. This program exercises both in the
	// same run, confirming indirectCalleePlace's runtime-field-identity
	// check (hardened in 50b3970, reused unchanged here) still correctly
	// tells them apart.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn main() int {\nvar t Table = Table.{ op = add };\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = t.op(3, 4);\nreturn *p;\n}", false, 7, false)
}

func TestEmitFunctionTypedParameterStructFieldArgumentCompilesAndRuns(t *testing.T) {
	// Passing a function-typed STRUCT FIELD as a call argument — combines
	// slice 2 (struct fields) and slice 3 (parameters).
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { var t Table = Table.{ op = add }; return apply(t.op, 1, 2); }", false, 3, false)
}

func TestEmitFunctionTypedParameterResultDoesNotRegressAllocator(t *testing.T) {
	// The same allocator-collision class slices 1 and 2 each guarded against,
	// exercised for parameters and results specifically.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int {\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\n*p = apply(add, 3, 4);\nreturn *p;\n}", false, 7, false)
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn main() int {\nlet allocator = context.default_allocator;\nlet p *int = allocator.alloc(allocator.ptr, sizeof int) as *int;\nvar f fn(int, int) int = chooseOp();\n*p = f(3, 4);\nreturn *p;\n}", false, 7, false)
}

func TestEmitU64FunctionTypeStructFieldCompilesAndRuns(t *testing.T) {
	// The motivating real-code shape, concretized without a generic struct:
	// a function-typed STRUCT FIELD whose signature returns u64 (hmap's
	// `hash_fn fn (K) u64`) and a separate struct field whose signature takes
	// u64, both called through the field. This is the position std/hmap.peb's
	// hash_fn field occupies.
	emitAndRun(t, "type Table = struct { hash fn(int) u64; }; fn hashOf(x int) u64 { return x as u64; } fn main() int { var t Table = Table.{ hash = hashOf }; var h u64 = t.hash(5); return h as int; }", false, 5, false)
	emitAndRun(t, "type Conv = struct { toi fn(u64) int; }; fn udToInt(x u64) int { return x as int; } fn main() int { var v u64 = 6; var c Conv = Conv.{ toi = udToInt }; return c.toi(v); }", false, 6, false)
}

func TestEmitPointerFunctionTypeStructFieldCompilesAndRuns(t *testing.T) {
	// A function-valued STRUCT FIELD whose signature takes a `*int` parameter
	// AND returns a `*int` result (`op fn(*int) *int`), constructed and called
	// through the field. This is the pointer-bearing mirror of slice 2's
	// `op fn(int, int) int` field test: the fnptr typedef is collected ahead
	// of the struct typedef that names it (slice 2's ordering rule), and both
	// pointer spellings must be correct for the -Wall -Wextra -Werror build.
	emitAndRun(t, "type Table = struct { op fn(*int) *int; }; fn identity(p *int) *int { return p; } fn main() int { var x int = 7; var t Table = Table.{ op = identity }; var p *int = t.op(&x); return *p; }", false, 7, false)
}
