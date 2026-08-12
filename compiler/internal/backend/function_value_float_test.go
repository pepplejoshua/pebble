package backend

import (
	"bytes"
	"strings"
	"testing"
)

// Phase 3 #9 — first-class function value float parameter/result widening.
// These tests prove, end to end through the compile-link-run harness, that a
// function type whose signature mentions f32/f64 in a parameter and/or result
// position is genuinely wireable as a first-class value: the function value is
// referenced (HoistedFunctionValue), stored into a function-typed local, and
// called indirectly through it, and the same value is also passed as an
// argument to a higher-order helper that calls it — the two positions the
// proposal's audit requires. The narrow-widening principle matches every other
// float pass in this backend (struct fields, optional payloads, slice
// elements, tagged-union payloads): only float is admitted here; aggregate
// parameters/results remain a separate, later follow-up.

// TestEmitFloatParamFunctionValueCompileAndRun proves a first-class function
// value with an f32/f64 PARAMETER is wireable (Phase 3 #9). Each case builds
// `var f fn(<float>) <float> = <helper>;` — the bare top-level function
// reference stored into a function-typed local — calls f through the local
// (the assigned-to-a-local position), and also passes the SAME function value
// as an argument to a higher-order helper `apply` whose function-typed
// parameter it calls (the passed-as-an-argument position). Both indirect calls
// exercise the float parameter grammar: the call-site argument is built by
// buildCallArgument's float case at the parameter's own kind and passed into a
// helper declared with the same floatCType the fnptr typedef names, so the
// value must arrive intact and round trip exactly. An exit code of 42 means
// both calls recovered the exact constructed result; 0 fires if either did
// not.
func TestEmitFloatParamFunctionValueCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"f64", "fn double_it(x f64) f64 { return x * 2.0; } fn apply_f64(f fn(f64) f64, x f64) f64 { return f(x); } fn main() int { let f fn(f64) f64 = double_it; var y f64 = f(21.0); var z f64 = apply_f64(f, 5.0); if y == 42.0 && z == 10.0 { return 42; } return 0; }"},
		{"f32", "fn triple_it(x f32) f32 { return x * 3.0; } fn apply_f32(f fn(f32) f32, x f32) f32 { return f(x); } fn main() int { let f fn(f32) f32 = triple_it; var y f32 = f(14.0); var z f32 = apply_f32(f, 2.0); if y == 42.0 && z == 6.0 { return 42; } return 0; }"},
		// A float parameter with a non-float RESULT: the widened parameter
		// admission must not force the result to be float too. The fnptr
		// typedef declares `int32_t (*...)(PebbleContext *ctx, double);` and
		// the indirect call's integer result is consumed by buildExpr's
		// IndirectCall path.
		{"f64 param int result", "fn to_whole(x f64) int { var n int = x as int; return n; } fn floor_f64(f fn(f64) int, x f64) int { return f(x); } fn main() int { let f fn(f64) int = to_whole; var a int = f(42.0); var b int = floor_f64(f, 99.0); if a == 42 && b == 99 { return 42; } return 0; }"},
		{"f32 param int result", "fn to_whole_f32(x f32) int { var n int = x as int; return n; } fn floor_f32(f fn(f32) int, x f32) int { return f(x); } fn main() int { let f fn(f32) int = to_whole_f32; var a int = f(42.0); var b int = floor_f32(f, 99.0); if a == 42 && b == 99 { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitFloatResultFunctionValueCompileAndRun proves a first-class function
// value with an f32/f64 RESULT is wireable (Phase 3 #9). Each case stores a
// no-parameter float-returning helper into a function-typed local and calls it
// indirectly (the assigned-to-a-local position), and also passes the value to
// a higher-order helper that calls it (the passed-as-an-argument position).
// Both indirect calls' float results are consumed as float values: a float
// local's declaration initializer and a comparison operand, each routed
// through buildFloatExpr's IndirectCall case. An exit code of 42 means both
// calls recovered the exact constructed float; 0 fires if either did not.
func TestEmitFloatResultFunctionValueCompileAndRun(t *testing.T) {
	cases := []struct {
		name   string
		source string
	}{
		{"f64", "fn two_point_five() f64 { return 2.5; } fn echo_f64(f fn() f64) f64 { return f(); } fn main() int { let f fn() f64 = two_point_five; var y f64 = f(); var z f64 = echo_f64(f); if y == 2.5 && z == 2.5 { return 42; } return 0; }"},
		{"f32", "fn one_point_five() f32 { return 1.5; } fn echo_f32(f fn() f32) f32 { return f(); } fn main() int { let f fn() f32 = one_point_five; var y f32 = f(); var z f32 = echo_f32(f); if y == 1.5 && z == 1.5 { return 42; } return 0; }"},
		// A float RESULT with a non-float parameter: the widened result
		// admission must not force the parameter to be float too. The fnptr
		// typedef declares `double (*...)(PebbleContext *ctx, int32_t);` and
		// each argument is built by buildCallArgument's integer case.
		{"f64 result int param", "fn as_f64(x int) f64 { return (x as f64); } fn lift_f64(f fn(int) f64, x int) f64 { return f(x); } fn main() int { let f fn(int) f64 = as_f64; var y f64 = f(42); var z f64 = lift_f64(f, 5); if y == 42.0 && z == 5.0 { return 42; } return 0; }"},
		{"f32 result int param", "fn as_f32(x int) f32 { return (x as f32); } fn lift_f32(f fn(int) f32, x int) f32 { return f(x); } fn main() int { let f fn(int) f32 = as_f32; var y f32 = f(7); var z f32 = lift_f32(f, 3); if y == 7.0 && z == 3.0 { return 42; } return 0; }"},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, 42, false)
		})
	}
}

// TestEmitFloatFunctionValueResultPositionsCompileAndRun proves a float-result
// indirect call's result is consumable in the float VALUE positions beyond a
// local initializer: forwarded as a float-returning helper's tail-position
// return (buildReturnStatement's float case), printed (buildPrint's float
// case), and passed as a float argument to an ordinary helper (buildCallArgument's
// float case). All three route through buildFloatExpr, so each is a distinct
// regression guard for the IndirectCall case added there. The tail-position
// return forward is the shape a float-returning helper built FROM a
// function-typed local must emit; printing proves a discarded-into-print
// float result builds; and the direct-helper argument proves the same float
// value flows through an ordinary (non-fnptr) call.
func TestEmitFloatFunctionValueResultPositionsCompileAndRun(t *testing.T) {
	// Tail-position return forward: `forward` takes a float-returning
	// function value and returns its indirect call's float result directly.
	emitAndRun(t, "fn seven() f64 { return 7.0; } fn forward(f fn() f64) f64 { return f(); } fn main() int { let f fn() f64 = seven; var y f64 = forward(f); if y == 7.0 { return 42; } return 0; }", false, 42, false)
	// Float result passed as an argument to an ordinary helper: the indirect
	// call's float result is built as a call argument by buildCallArgument's
	// float case, not consumed by buildFloatExpr directly.
	emitAndRun(t, "fn seven() f32 { return 7.0; } fn add_half(x f32) f32 { return x + 0.5; } fn main() int { let f fn() f32 = seven; var y f32 = add_half(f()); if y == 7.5 { return 42; } return 0; }", false, 42, false)
	// Print of an indirect call's float result: the operand is built by
	// buildFloatExpr (print's float case), so the IndirectCall case must
	// produce a valid float expression there too.
	emitAndRun(t, "fn nine() f64 { return 9.0; } fn main() int { let f fn() f64 = nine; print f(); return 42; }", false, 42, false)
}

// TestEmitFloatFunctionTypeWritesC confirms the emitted C directly for the
// widened signature: the fnptr typedef declares the float parameter and/or
// result at the plain C float/double types (floatCType) the hoisted helper is
// declared with, so a bare function name decays to exactly the typedef's
// pointer shape. The f64 case names `double` for both the result and the
// parameter; the f32 case names `float` for both; and the mixed case (float
// param, integer result) names `int32_t` for the result while the parameter
// stays `double` — proving the two admission axes widened independently.
func TestEmitFloatFunctionTypeWritesC(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   []string
	}{
		{"f64 param result", "fn double_it(x f64) f64 { return x * 2.0; } fn main() int { var f fn(f64) f64 = double_it; var y f64 = f(21.0); if y == 42.0 { return 42; } return 0; }", []string{"typedef double (*pebble_fnptr_", ")(PebbleContext *ctx, double);", "double pebble_fn_"}},
		{"f32 param result", "fn triple_it(x f32) f32 { return x * 3.0; } fn main() int { var f fn(f32) f32 = triple_it; var y f32 = f(14.0); if y == 42.0 { return 42; } return 0; }", []string{"typedef float (*pebble_fnptr_", ")(PebbleContext *ctx, float);", "float pebble_fn_"}},
		{"f64 param int result", "fn to_whole(x f64) int { var n int = x as int; return n; } fn main() int { var f fn(f64) int = to_whole; return f(42.0); }", []string{"typedef int32_t (*pebble_fnptr_", ")(PebbleContext *ctx, double);", "int32_t pebble_fn_"}},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			unit, snapshot, entryID, sources := buildFixture(t, tc.source, "main", false)
			var buf bytes.Buffer
			if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
				t.Fatalf("Emit failed: %v", err)
			}
			out := buf.String()
			for _, want := range tc.want {
				if !strings.Contains(out, want) {
					t.Errorf("emitted C missing %q:\n%s", want, out)
				}
			}
		})
	}
}
