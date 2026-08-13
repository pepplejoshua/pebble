package backend

import (
	"testing"
)

func TestEmitInterpolatedStringLiteralOnlyAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with only literal text parts (no value parts) used
	// as a str-typed local's declaration initializer — `let s str = \`hello\`;`
	// — must materialize into a PebbleStr whose bytes are exactly the literal
	// text. We verify by comparing the local against a plain string literal
	// (which exercises pebble_rt_str_eq under the hood).
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"single text part", "fn main() i32 { let s str = `hello`; if s == \"hello\" { return 0; } return 1; }", 0},
		{"text with spaces", "fn main() i32 { let s str = `hello world`; if s == \"hello world\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringWithBoolPartsAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with bool value parts interleaved with text used
	// as a str-typed local's declaration initializer. Each bool part must be
	// formatted as "true" or "false" and concatenated with surrounding text
	// parts into a single PebbleStr. We verify by comparing the local against
	// a plain string literal.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"bool true", "fn main() i32 { let b bool = true; let s str = `flag={b}`; if s == \"flag=true\" { return 0; } return 1; }", 0},
		{"bool false", "fn main() i32 { let b bool = false; let s str = `flag={b}`; if s == \"flag=false\" { return 0; } return 1; }", 0},
		{"multiple bools", "fn main() i32 { let a bool = true; let b bool = false; let s str = `{a},{b}`; if s == \"true,false\" { return 0; } return 1; }", 0},
		{"bool with surrounding text", "fn main() i32 { let b bool = true; let s str = `before {b} after`; if s == \"before true after\" { return 0; } return 1; }", 0},
		{"bool expression", "fn main() i32 { let b bool = false; let s str = `result={!b}`; if s == \"result=true\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string used as a call argument for a str parameter —
	// `takes(`hello {b}`)` — must materialize into a PebbleStr value that
	// flows through the call to the callee. We verify by having the callee
	// compare the received value against an expected literal.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"literal-only arg", "fn takes(s str) i32 { if s == \"hello\" { return 1; } return 0; }\nfn main() i32 { return takes(`hello`); }", 1},
		{"bool arg", "fn takes(s str) i32 { if s == \"ok=true\" { return 1; } return 0; }\nfn main() i32 { let b bool = true; return takes(`ok={b}`); }", 1},
		{"bool false arg", "fn takes(s str) i32 { if s == \"ok=false\" { return 1; } return 0; }\nfn main() i32 { let b bool = false; return takes(`ok={b}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string used as a tail-position return value from a
	// str-returning helper — `fn make() str { return \`hi\`; }` — must
	// materialize into a PebbleStr that is returned to the caller. We verify
	// by comparing the returned value against a literal in main.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"literal-only return", "fn make() str { return `hello`; }\nfn main() i32 { let s str = make(); if s == \"hello\" { return 0; } return 1; }", 0},
		{"bool return", "fn make(b bool) str { return `val={b}`; }\nfn main() i32 { let s str = make(true); if s == \"val=true\" { return 0; } return 1; }", 0},
		{"bool false return", "fn make(b bool) str { return `val={b}`; }\nfn main() i32 { let s str = make(false); if s == \"val=false\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string used directly in a comparison expression —
	// `if `hello {b}` == "hello true" { ... }` — must materialize into a
	// PebbleStr that participates in pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "fn main() i32 { let b bool = true; if `prefix={b}` == \"prefix=true\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "fn main() i32 { let b bool = true; if `prefix={b}` == \"prefix=false\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string —
	// `var s str = "initial"; s = `new={b}`;` — must materialize the
	// interpolated string into a PebbleStr and store it into the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign literal-only", "fn main() i32 { var s str = \"old\"; s = `new`; if s == \"new\" { return 0; } return 1; }", 0},
		{"reassign with bool", "fn main() i32 { var s str = \"old\"; let b bool = true; s = `v={b}`; if s == \"v=true\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringPrintUnaffected(t *testing.T) {
	t.Parallel()
	// Existing print-path behavior must remain unchanged: an interpolated
	// string used directly as a print operand still lowers to the combined
	// printf path, not through materialization. This confirms our changes
	// to buildStrOperand/buildStrLocalDeclaration did not regress the
	// buildPrint InterpolatedString handling.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print literal-only", "fn main() i32 { print `hello`; return 0; }", "hello\n"},
		{"print with bool", "fn main() i32 { let b bool = true; print `b={b}`; return 0; }", "b=true\n"},
		{"print with bool false", "fn main() i32 { let b bool = false; print `b={b}`; return 0; }", "b=false\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
}
