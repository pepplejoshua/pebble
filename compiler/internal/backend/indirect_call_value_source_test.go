package backend

import (
	"bytes"
	"strings"
	"testing"
)

// Phase 3 #16 — indirect-call CALL-SITE value-source coverage. The proposal 14
// "Indirect call" row's "Partial by function signature" wording is explained
// by the function-type signature restriction tracked on the "Function type and
// function value" row (aggregate parameters/results), but the CALL SITE has a
// distinct, previously-uncovered family of shapes: an indirect call whose
// callee is a function value read off a NON-ADDRESSABLE struct value (a call
// result), and an indirect call's result feeding the value-source positions
// established in Phase 3 #11. These tests pin both the shapes that already
// worked (callee from a call result directly; results in argument/field-value
// positions; sequential indirect calls) and the ones fixed here (a
// function-typed field of a call result as callee / local init / call
// argument / struct field construction value — buildFunctionValue's FieldValue
// case now routes a non-local receiver through buildStructValueNode).

func TestEmitIndirectCallCalleeFromCallResultCompileAndRun(t *testing.T) {
	t.Parallel()
	// The callee itself is a call result returning a function value, called
	// directly with no local binding: chooseOp()(1, 2). buildFunctionValue's
	// DirectCall case builds the callee with buildDirectCall (a function
	// pointer value of the exact fnptr typedef type), so calling it with the
	// threaded context is valid C. Confirmed working before the fix; proof
	// test added for this row's callee-source matrix.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn chooseOp() fn(int, int) int { return add; } fn main() int { return chooseOp()(1, 2); }", false, 3, false)
}

func TestEmitIndirectCallCalleeFromStructFieldOfCallResultCompileAndRun(t *testing.T) {
	t.Parallel()
	// An indirect call through a function-typed struct field whose receiver is
	// a NON-ADDRESSABLE struct VALUE — the struct returned by a call
	// (mk().op(1, 2)), read by the checker as a bare FieldValue whose single
	// child is a DirectCall. The backend previously rejected this ("reads a
	// function-typed field from a DirectCall receiver"); buildFunctionValue's
	// FieldValue case now routes a non-local receiver through
	// buildStructValueNode, the same struct-VALUE builder the struct-typed
	// field-read path (Phase 3 #11) uses.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn mk() Table { return Table.{ op = add }; } fn main() int { return mk().op(1, 2); }", false, 3, false)
}

func TestEmitIndirectCallStructFieldOfCallResultAsLocalInitCompileAndRun(t *testing.T) {
	t.Parallel()
	// The same field-of-call-result value in a function-typed local's
	// declaration initializer: `var f fn(int, int) int = mk().op;`.
	// buildFunctionValue's FieldValue case handles the same shape in every
	// value position; this is the local-declaration position.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn mk() Table { return Table.{ op = add }; } fn main() int { var f fn(int, int) int = mk().op; return f(1, 2); }", false, 3, false)
}

func TestEmitIndirectCallStructFieldOfCallResultAsArgumentCompileAndRun(t *testing.T) {
	t.Parallel()
	// The same field-of-call-result value passed as a function-typed CALL
	// ARGUMENT: apply(mk().op, 1, 2). buildCallArgument's function-value case
	// routes through buildFunctionValue, so the same FieldValue handling
	// fixes the argument position.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn mk() Table { return Table.{ op = add }; } fn apply(f fn(int, int) int, x int, y int) int { return f(x, y); } fn main() int { return apply(mk().op, 1, 2); }", false, 3, false)
}

func TestEmitIndirectCallStructFieldOfCallResultAsFieldValueCompileAndRun(t *testing.T) {
	t.Parallel()
	// The same field-of-call-result value used as a struct field's
	// construction value: Box.{ f = mk().op }, then called back through the
	// field. buildStructValueExpr's function-typed field-value case routes
	// through buildFunctionValue, so the same FieldValue handling fixes the
	// record-construction position too.
	emitAndRun(t, "type Table = struct { op fn(int, int) int; }; type Box = struct { f fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn mk() Table { return Table.{ op = add }; } fn main() int { var b Box = Box.{ f = mk().op }; return b.f(3, 4); }", false, 7, false)
}

func TestEmitIndirectCallStructFieldOfCallResultWritesC(t *testing.T) {
	t.Parallel()
	// Pin the emitted C for the fixed shape: the call-result field callee is
	// built as the call expression (pebble_fn_mk(ctx)) projected with the
	// designated-field name, and the indirect call threads ctx and its
	// arguments through the field's own fnptr value.
	unit, snapshot, entryID, sources := buildFixture(t, "type Table = struct { op fn(int, int) int; }; fn add(a int, b int) int { return a + b; } fn mk() Table { return Table.{ op = add }; } fn main() int { return mk().op(1, 2); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, ".pebble_field_") || !strings.Contains(out, "pebble_field_25(ctx, 1LL, 2LL)") {
		t.Errorf("emitted C missing the call-result field callee shape:\n%s", out)
	}
}

func TestEmitIndirectCallResultInValueSourcePositionsCompileAndRun(t *testing.T) {
	t.Parallel()
	// An indirect call's result feeding the value-source positions fixed in
	// Phase 3 #11: another call's argument, a struct field's construction
	// value, and a second indirect call's argument through the same callee.
	// All three shapes already worked (the float-result argument variant is
	// covered in function_value_float_test.go); these pin the int-width
	// forms in one compile-link-run matrix.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn triple(x int) int { return x * 3; } fn main() int { var f fn(int, int) int = add; return triple(f(1, 2)); }", false, 9, false)
	emitAndRun(t, "type Box = struct { x int; }; fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; var b Box = Box.{ x = f(3, 4) }; return b.x; }", false, 7, false)
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; return f(f(1, 2), f(3, 4)); }", false, 10, false)
}

func TestEmitSequentialIndirectCallsThroughSameLocalCompileAndRun(t *testing.T) {
	t.Parallel()
	// Multiple sequential indirect calls through the same function-typed
	// local (f(1, 2) then f(3, 4)) — the local keeps its function value across
	// both calls, proving no stale state between indirect calls.
	emitAndRun(t, "fn add(a int, b int) int { return a + b; } fn main() int { var f fn(int, int) int = add; var a int = f(1, 2); var b int = f(3, 4); return a + b; }", false, 10, false)
}
