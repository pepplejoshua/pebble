package backend

import (
	"bytes"
	"fmt"
	"strings"
	"testing"
)

// TestEmitGlobalReadInitialValueCompilesAndRuns proves a mutable (`var`)
// module-level global's initial value is observable: `var counter int = 5;`
// must emit real file-scope storage seeded with 5 (`static int32_t
// pebble_global_<id> = 5;`), and `return counter;` must read that storage
// back — this is the exact read reproduction from the parity-gap tracker.
func TestEmitGlobalReadInitialValueCompilesAndRuns(t *testing.T) {
	emitAndRun(t, "var counter int = 5;\n\nfn main() int {\n    return counter;\n}", false, 5, false)
}

// TestEmitGlobalReadInitialValueWritesCStorage pins the emitted C shape: the
// global's file-scope declaration must carry the initializer value (not a
// zero placeholder) and the read must resolve to that storage's C name.
func TestEmitGlobalReadInitialValueWritesCStorage(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "var counter int = 5;\n\nfn main() int {\n    return counter;\n}", "main", false)
	var globalID uint32
	for _, g := range unit.GlobalDeclarations() {
		globalID = uint32(g.Symbol)
	}
	if globalID == 0 {
		t.Fatal("fixture has no global declaration")
	}
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		fmt.Sprintf("static int32_t pebble_global_%d = 5;", globalID),
		fmt.Sprintf("return pebble_global_%d;", globalID),
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// TestEmitGlobalWriteAcrossFunctionsCompilesAndRuns proves a global write in
// one function is observed by a read in a different function — real shared
// mutable state, not just that each operation individually compiles. bump
// increments the global twice; read() returns it; main returns read().
func TestEmitGlobalWriteAcrossFunctionsCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var counter int = 0;

fn bump() void {
    counter = counter + 1;
}

fn read() int {
    return counter;
}

fn main() int {
    bump();
    bump();
    return read();
}`, false, 2, false)
}

// TestEmitGlobalBumpAcrossFunctionBoundariesCompilesAndRuns is the tracker's
// exact write reproduction: bump() reads, increments, and re-stores the global
// three times, and main returns the third call's result (0 -> 1 -> 2 -> 3).
func TestEmitGlobalBumpAcrossFunctionBoundariesCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var counter int = 0;

fn bump() int {
    counter = counter + 1;
    return counter;
}

fn main() int {
    bump();
    bump();
    return bump();
}`, false, 3, false)
}

// TestEmitGlobalReadWriteInLoopCompilesAndRuns exercises a global read/write
// inside a loop: any accidental per-iteration re-declaration of the storage
// would reset the counter each pass, but the loop must accumulate the global
// to 5. Bounded harness so a miscompiled non-terminating loop fails loudly.
func TestEmitGlobalReadWriteInLoopCompilesAndRuns(t *testing.T) {
	emitAndRunBounded(t, `var counter int = 0;

fn main() int {
    var i int = 0;
    while i < 5 {
        counter = counter + 1;
        i = i + 1;
    }
    return counter;
}`, false, 5, false)
}

// TestEmitGlobalCompoundAssignmentCompilesAndRuns covers the compound-assign
// path for a global place (`counter += 1;`), which resolves through
// buildCompoundStore's global branch.
func TestEmitGlobalCompoundAssignmentCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var counter int = 10;

fn main() int {
    counter += 5;
    return counter;
}`, false, 15, false)
}

// TestEmitBoolGlobalCompilesAndRuns covers a bool-typed global (read path via
// buildBoolExpr).
func TestEmitBoolGlobalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var flag bool = true;

fn main() int {
    if flag {
        return 7;
    }
    return 0;
}`, false, 7, false)
}

// TestEmitCharGlobalCompilesAndRuns covers a char-typed global (read path via
// buildCharOperand).
func TestEmitCharGlobalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var c char = 'a';

fn main() int {
    if c == 'a' {
        return 4;
    }
    return 0;
}`, false, 4, false)
}

// TestEmitUintGlobalCompilesAndRuns covers a uint-typed global (read path via
// buildUintExpr).
func TestEmitUintGlobalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var n uint = 3;

fn main() int {
    if n == 3 {
        return 9;
    }
    return 0;
}`, false, 9, false)
}

// TestEmitEnumGlobalCompilesAndRuns covers an enum-typed global (read path via
// buildEnumValue; the enum typedef must precede the global's storage
// declaration in the emitted C).
func TestEmitEnumGlobalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `type Color = enum { red, green, blue };

var c Color = Color.green;

fn main() int {
    if c == Color.green {
        return 11;
    }
    return 0;
}`, false, 11, false)
}

// TestEmitStrGlobalCompilesAndRuns covers a str-typed global: its storage is a
// PebbleStr static initializer and it can be read (comparison) and reassigned
// from a string literal.
func TestEmitStrGlobalCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `var s str = "hi";

fn main() int {
    if s == "hi" {
        s = "bye";
        if s == "bye" {
            return 12;
        }
    }
    return 0;
}`, false, 12, false)
}

// TestEmitGlobalUnusedDoesNotEmitStorage guards against emitting file-scope
// storage for a `var` global the reachable program never references: an unused
// static declaration would trip -Wunused-variable under the mandated
// -Wall -Wextra -Werror build, so the program must still compile and run.
func TestEmitGlobalUnusedDoesNotEmitStorage(t *testing.T) {
	emitAndRun(t, "var x int = 5;\n\nfn main() int {\n    return 0;\n}", false, 0, false)
}

// TestEmitGlobalNonLiteralInitializerRejected guards the flagged design
// boundary: a constant initializer that is not a literal leaf (checked
// arithmetic over literals) is not a C static-initializable expression, so the
// backend rejects it cleanly instead of emitting a runtime call into a static
// initializer.
func TestEmitGlobalNonLiteralInitializerRejected(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, "var x int = 1 + 2;\n\nfn main() int {\n    return x;\n}", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err == nil {
		t.Fatal("Emit accepted a non-literal global initializer")
	} else if !strings.Contains(err.Error(), "not a literal constant") {
		t.Fatalf("unexpected rejection: %v", err)
	}
}
