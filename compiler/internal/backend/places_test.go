package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitPointerAddressOfAndDerefCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// var y i32 = 5; let p *i32 = &y; return *p;
	// Takes the address of a local and dereferences it, proving the pointer
	// round-trip works end-to-end.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; return *p; }", false, 5, false)
}

func TestEmitWriteThroughPointerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// var y i32 = 5; let p *i32 = &y; *p = 9; return y;
	// Writes through a pointer and reads back through the original variable,
	// proving the pointer genuinely aliases y, not a copy.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; *p = 9; return y; }", false, 9, false)
}

func TestEmitPointerToPointerCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// var y i32 = 7; let p *i32 = &y; let q *i32 = p; return *q;
	// Pointer-to-pointer copy, then dereference through the copy.
	emitAndRun(t, "fn main() i32 { var y i32 = 7; let p *i32 = &y; let q *i32 = p; return *q; }", false, 7, false)
}

func TestEmitPointerEmittedCContainsCheckedDeref(t *testing.T) {
	t.Parallel()
	// Verify the emitted C contains pebble_rt_checked_deref_ptr calls for
	// dereference operations, not raw C dereferences.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var y i32 = 5; let p *i32 = &y; return *p; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_rt_checked_deref_ptr") {
		t.Errorf("emitted C missing pebble_rt_checked_deref_ptr call:\n%s", out)
	}
	compileAndRun(t, buf.Bytes(), 5, false)
}

func TestEmitNullDerefReadPanics(t *testing.T) {
	t.Parallel()
	// A pointer whose value is nil at runtime, dereferenced on the read side.
	// The null value is produced indirectly so the checker cannot reject it at
	// compile time: a helper stores nil in a local and returns it.
	// fn getNullPtr() *i32 { let p *i32 = nil; return p; }
	// fn main() i32 { let p *i32 = getNullPtr(); return *p; }
	// The dereference must panic with PEBBLE_PANIC_NULL_DEREFERENCE.
	emitAndRun(t, "fn getNullPtr() *i32 { let p *i32 = nil; return p; } fn main() i32 { let p *i32 = getNullPtr(); return *p; }", false, 0, true)
}

func TestEmitNullDerefWritePanics(t *testing.T) {
	t.Parallel()
	// Same shape but assigning through the null pointer.
	// fn getNullPtr() *i32 { let p *i32 = nil; return p; }
	// fn main() i32 { let p *i32 = getNullPtr(); *p = 42; return 0; }
	emitAndRun(t, "fn getNullPtr() *i32 { let p *i32 = nil; return p; } fn main() i32 { let p *i32 = getNullPtr(); *p = 42; return 0; }", false, 0, true)
}
