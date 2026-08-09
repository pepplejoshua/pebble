package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitOpaqueExternTypeWritesRealCTypeName(t *testing.T) {
	// The emitted-C shape assertion: *FILE must be the real `FILE *`, never
	// a synthesized pebble_struct_<id>_t, and no bogus empty struct typedef
	// for FILE may be emitted (collectStructTypes must exclude an opaque
	// extern type from the struct-typedef machinery entirely).
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `extern {
    type FILE;
    fn fopen(path str, mode str) *FILE;
    fn fclose(file *FILE) i32;
}
fn main() int {
    var f = fopen("test.txt", "r");
    fclose(f);
    return 0;
}`)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, resolution, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	emitted := buf.String()
	if !strings.Contains(emitted, "FILE *") {
		t.Errorf("emitted C does not declare FILE *:\n%s", emitted)
	}
	if strings.Contains(emitted, "pebble_struct_") {
		t.Errorf("emitted C contains a synthesized pebble_struct_ typedef for the opaque extern type FILE, want none:\n%s", emitted)
	}
	if !strings.Contains(emitted, "fopen((const char *)") {
		t.Errorf("emitted C does not lower fopen's str arguments to const char *:\n%s", emitted)
	}
	if strings.Contains(emitted, "fopen((PebbleStr)") {
		t.Errorf("emitted C passes a PebbleStr struct to fopen, want const char *:\n%s", emitted)
	}
	compileAndRun(t, buf.Bytes(), 0, false)
}

func TestEmitStructWithUintFieldWritesUint64T(t *testing.T) {
	// Emitted-C shape check: the uint field's typedef must declare uint64_t,
	// not the entry width's own C type or a rejection.
	unit, snapshot, entryID, sources := buildFixture(t, "type Counter = struct { n uint; }; fn main() i32 { var c Counter = Counter.{ n = 5 }; return c.n as i32; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "uint64_t pebble_field_") {
		t.Errorf("expected a uint64_t struct field in emitted C, got:\n%s", out)
	}
}

func TestEmitU8SliceWritesUint8CType(t *testing.T) {
	// Emitted-C shape check: a []u8 array/slice pair must declare uint8_t,
	// not the ambient entry width's C type (int32_t) or any other width —
	// arrayElementCType's scalar fallback previously returned cType(width)
	// (the AMBIENT width), which would have been silently wrong here rather
	// than a clean rejection; this confirms the element's OWN resolved width
	// is what's actually emitted.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() int { var arr [3]u8 = [1 as u8, 2 as u8, 3 as u8]; var s []u8 = arr[:]; return s[1] as int; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "uint8_t") {
		t.Errorf("expected uint8_t in emitted C, got:\n%s", out)
	}
}
