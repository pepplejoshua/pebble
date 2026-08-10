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
	t.Parallel()
	emitAndRun(t, "var counter int = 5;\n\nfn main() int {\n    return counter;\n}", false, 5, false)
}

// TestEmitGlobalReadInitialValueWritesCStorage pins the emitted C shape: the
// global's file-scope declaration must carry the initializer value (not a
// zero placeholder) and the read must resolve to that storage's C name.
func TestEmitGlobalReadInitialValueWritesCStorage(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	emitAndRun(t, `var counter int = 10;

fn main() int {
    counter += 5;
    return counter;
}`, false, 15, false)
}

// TestEmitBoolGlobalCompilesAndRuns covers a bool-typed global (read path via
// buildBoolExpr).
func TestEmitBoolGlobalCompilesAndRuns(t *testing.T) {
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
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
	t.Parallel()
	emitAndRun(t, "var x int = 5;\n\nfn main() int {\n    return 0;\n}", false, 0, false)
}

// TestEmitGlobalConstantArithmeticFoldsCompilesAndRuns proves backend-side
// constant folding: a CheckedArithmetic over pure integer literals (`var x int
// = 1 + 2;`) folds to a plain literal C constant and the program returns the
// folded value, exactly the reproduction from the parity-gap tracker.
func TestEmitGlobalConstantArithmeticFoldsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "var x int = 1 + 2;\n\nfn main() int {\n    return x;\n}", false, 3, false)
}

// TestEmitGlobalConstantArithmeticFoldsOperatorsCompilesAndRuns covers a
// second operator (*) and the / and % operators, whose truncated division and
// remainder semantics the folder reproduces with big.Int's Quo/Rem: 10 / 3 =
// 3, 10 % 3 = 1, so the second global folds to 4.
func TestEmitGlobalConstantArithmeticFoldsOperatorsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "var x int = 6 * 7;\n\nfn main() int {\n    return x;\n}", false, 42, false)
	emitAndRun(t, "var y int = 10 / 3 + 10 % 3;\n\nfn main() int {\n    return y;\n}", false, 4, false)
}

// TestEmitGlobalConstantArithmeticFoldsToLiteralText pins the emitted C shape:
// the folded value must land in the storage declaration as a plain literal
// (`static int32_t pebble_global_<id> = 42;`), not a runtime call.
func TestEmitGlobalConstantArithmeticFoldsToLiteralText(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "var x int = 6 * 7;\n\nfn main() int {\n    return x;\n}", "main", false)
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
		fmt.Sprintf("static int32_t pebble_global_%d = 42;", globalID),
		fmt.Sprintf("return pebble_global_%d;", globalID),
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

// TestEmitGlobalConstantArithmeticOverflowRejected proves a folded result that
// does not fit the global's declared type range is a clean, distinct rejection
// naming the overflowing value — not a Go panic, not a silent wrap, and not the
// generic "not a literal constant" message. 250 + 10 folds to 260, outside
// u8's 0..255 range.
func TestEmitGlobalConstantArithmeticOverflowRejected(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "var x u8 = 250 + 10;\n\nfn main() int {\n    if x == 255 {\n        return 1;\n    }\n    return 0;\n}", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err == nil {
		t.Fatal("Emit accepted a folded global initializer that overflows its type")
	} else if !strings.Contains(err.Error(), "outside the global's u8 type range") {
		t.Fatalf("unexpected rejection: %v", err)
	} else if !strings.Contains(err.Error(), "260") {
		t.Fatalf("rejection does not name the overflowing folded value: %v", err)
	} else if strings.Contains(err.Error(), "not a literal constant") {
		t.Fatalf("overflow must be a distinct rejection, not the generic one: %v", err)
	}
}

// TestEmitGlobalNonLiteralInitializerRejected guards the flagged design
// boundary: a compile-time-constant initializer that is NOT a foldable
// integer-literal arithmetic tree is not a C static-initializable expression,
// so the backend rejects it with the exact "not a literal constant" message
// instead of emitting a runtime call into a static initializer. The fixture is
// a CheckedNegate (`-5`), which the checker accepts as a constant but the
// folder correctly declines; a CheckedArithmetic tree with any such
// non-literal operand (`1 + -5`) falls back to the same rejection.
func TestEmitGlobalNonLiteralInitializerRejected(t *testing.T) {
	t.Parallel()
	for _, fixture := range []string{
		"var x int = -5;\n\nfn main() int {\n    return x;\n}",
		"var x int = 1 + -5;\n\nfn main() int {\n    return x;\n}",
	} {
		unit, snapshot, entryID, sources := buildFixture(t, fixture, "main", false)
		var buf bytes.Buffer
		if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err == nil {
			t.Fatal("Emit accepted a non-foldable global initializer")
		} else if !strings.Contains(err.Error(), "not a literal constant") {
			t.Fatalf("unexpected rejection: %v", err)
		}
	}
}
