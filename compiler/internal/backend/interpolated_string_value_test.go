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

func TestEmitInterpolatedStringWithIntPartsAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with integer value parts (any builtin width)
	// used as a str-typed local's declaration initializer. Each integer part
	// must be formatted as its decimal representation (a leading '-' for a
	// negative signed value) and concatenated with surrounding text parts
	// into a single PebbleStr. A u64 near its max value proves the
	// formatting has no width-based truncation or overflow, and the narrow
	// widths prove the VALUE is formatted, not the storage width.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"positive int", "fn main() i32 { let x int = 42; let s str = `n={x}`; if s == \"n=42\" { return 0; } return 1; }", 0},
		{"negative i64", "fn main() i32 { let x i64 = -9223372036854775808; let s str = `n={x}`; if s == \"n=-9223372036854775808\" { return 0; } return 1; }", 0},
		{"unsigned zero", "fn main() i32 { let x u64 = 0; let s str = `n={x}`; if s == \"n=0\" { return 0; } return 1; }", 0},
		{"large u64 near max", "fn main() i32 { let x u64 = 18446744073709551615; let s str = `n={x}`; if s == \"n=18446744073709551615\" { return 0; } return 1; }", 0},
		{"narrow i8", "fn main() i32 { let x i8 = -128; let s str = `n={x}`; if s == \"n=-128\" { return 0; } return 1; }", 0},
		{"narrow u8", "fn main() i32 { let x u8 = 255; let s str = `n={x}`; if s == \"n=255\" { return 0; } return 1; }", 0},
		{"int with surrounding text", "fn main() i32 { let x int = 42; let s str = `before {x} after`; if s == \"before 42 after\" { return 0; } return 1; }", 0},
		{"multiple int parts", "fn main() i32 { let a int = 1; let b i64 = 2; let c u64 = 3; let s str = `{a},{b},{c}`; if s == \"1,2,3\" { return 0; } return 1; }", 0},
		{"int and bool parts", "fn main() i32 { let n int = 5; let b bool = true; let s str = `n={n},b={b}`; if s == \"n=5,b=true\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringIntAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with an integer value part used as a call
	// argument for a str parameter — `takes(`ok={x}`)` — must materialize
	// into a PebbleStr value that flows through the call to the callee.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"int arg", "fn takes(s str) i32 { if s == \"ok=42\" { return 1; } return 0; }\nfn main() i32 { let x int = 42; return takes(`ok={x}`); }", 1},
		{"negative int arg", "fn takes(s str) i32 { if s == \"ok=-5\" { return 1; } return 0; }\nfn main() i32 { let x i32 = -5; return takes(`ok={x}`); }", 1},
		{"u64 max arg", "fn takes(s str) i32 { if s == \"ok=18446744073709551615\" { return 1; } return 0; }\nfn main() i32 { let x u64 = 18446744073709551615; return takes(`ok={x}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringIntAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with an integer value part used as a
	// tail-position return value from a str-returning helper — `fn make(x
	// int) str { return \`val={x}\`; }` — must materialize into a PebbleStr
	// that is returned to the caller.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"int return", "fn make(x int) str { return `val={x}`; }\nfn main() i32 { let s str = make(-42); if s == \"val=-42\" { return 0; } return 1; }", 0},
		{"u64 max return", "fn make(x u64) str { return `val={x}`; }\nfn main() i32 { let s str = make(18446744073709551615); if s == \"val=18446744073709551615\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringIntInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with an integer value part used directly in a
	// comparison expression — `if `prefix={n}` == "prefix=42" { ... }` —
	// must materialize into a PebbleStr that participates in
	// pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "fn main() i32 { let n i32 = 42; if `prefix={n}` == \"prefix=42\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "fn main() i32 { let n i32 = 42; if `prefix={n}` == \"prefix=43\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringIntReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string with an
	// integer value part — `var s str = "initial"; s = `new={n}`;` — must
	// materialize the interpolated string into a PebbleStr and store it into
	// the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign with int", "fn main() i32 { var s str = \"old\"; let n u64 = 7; s = `v={n}`; if s == \"v=7\" { return 0; } return 1; }", 0},
		{"reassign with negative int", "fn main() i32 { var s str = \"old\"; let n i64 = -9000000000; s = `v={n}`; if s == \"v=-9000000000\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringIntPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with integer value parts used directly as a
	// print operand must fold into the combined printf path exactly as a
	// bare integer print operand does, formatting each integer with its own
	// exact-width PRI* specifier.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print positive int", "fn main() i32 { let x int = 42; print `b={x}`; return 0; }", "b=42\n"},
		{"print negative int", "fn main() i32 { let x i32 = -7; print `b={x}`; return 0; }", "b=-7\n"},
		{"print unsigned zero", "fn main() i32 { let x u64 = 0; print `b={x}`; return 0; }", "b=0\n"},
		{"print large u64", "fn main() i32 { let x u64 = 18446744073709551615; print `b={x}`; return 0; }", "b=18446744073709551615\n"},
		{"print narrow u8", "fn main() i32 { let x u8 = 255; print `b={x}`; return 0; }", "b=255\n"},
		{"print narrow i8", "fn main() i32 { let x i8 = -128; print `b={x}`; return 0; }", "b=-128\n"},
		{"print multiple ints with text", "fn main() i32 { let a int = 1; let b i64 = -2; print `{a} and {b}`; return 0; }", "1 and -2\n"},
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
