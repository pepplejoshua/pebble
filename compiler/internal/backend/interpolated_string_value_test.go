package backend

import (
	"bytes"
	"fmt"
	"strings"
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

func TestEmitInterpolatedStringWithFloatPartsAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with float value parts (f32 or f64) used as a
	// str-typed local's declaration initializer. Each float part must be
	// formatted with %f's default precision (6 decimal digits — the same
	// convention buildPrint's bare scalar float print path uses, so an
	// interpolated float renders identically to a directly-printed float), a
	// leading '-' for a negative value, and concatenated with surrounding
	// text parts into a single PebbleStr. The large-exponent f64 cases
	// (1e300's %f rendering is 302 integer digits plus ".000000", far beyond
	// any integer part and near the runtime's per-part scratch-buffer bound —
	// the Go-level analog of the smoke test's DBL_MAX check) are generated
	// from a Go fmt.Sprintf reference rather than hand-written literals.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"positive f64", "fn main() i32 { let x f64 = 3.5; let s str = `n={x}`; if s == \"n=3.500000\" { return 0; } return 1; }", 0},
		{"negative f64", "fn main() i32 { let x f64 = -2.25; let s str = `n={x}`; if s == \"n=-2.250000\" { return 0; } return 1; }", 0},
		{"zero f64", "fn main() i32 { let x f64 = 0.0; let s str = `n={x}`; if s == \"n=0.000000\" { return 0; } return 1; }", 0},
		{"positive f32", "fn main() i32 { let x f32 = 0.5; let s str = `n={x}`; if s == \"n=0.500000\" { return 0; } return 1; }", 0},
		{"negative f32", "fn main() i32 { let x f32 = -3.5; let s str = `n={x}`; if s == \"n=-3.500000\" { return 0; } return 1; }", 0},
		{"very small f64", "fn main() i32 { let x f64 = 1.0e-10; let s str = `n={x}`; if s == \"n=0.000000\" { return 0; } return 1; }", 0},
		{"very small f32", "fn main() i32 { let x f32 = 1.0e-10; let s str = `n={x}`; if s == \"n=0.000000\" { return 0; } return 1; }", 0},
		{"very large f32", "fn main() i32 { let x f32 = 1.8446744073709552e19; let s str = `n={x}`; if s == \"n=18446744073709551616.000000\" { return 0; } return 1; }", 0},
		{"float with surrounding text", "fn main() i32 { let x f64 = -3.5; let s str = `before {x} after`; if s == \"before -3.500000 after\" { return 0; } return 1; }", 0},
		{"multiple float parts", "fn main() i32 { let a f32 = 1.25; let b f64 = 2.5; let s str = `{a}:{b}`; if s == \"1.250000:2.500000\" { return 0; } return 1; }", 0},
		{"float, bool, and int parts", "fn main() i32 { let f f64 = -3.5; let b bool = true; let n int = 42; let s str = `v={f},b={b},i={n}`; if s == \"v=-3.500000,b=true,i=42\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
	for _, tc := range []struct {
		name string
		lit  string
		v    float64
	}{
		{"very large f64", "1.0e300", 1.0e300},
		{"very large negative f64", "-1.0e300", -1.0e300},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			want := "n=" + fmt.Sprintf("%f", tc.v)
			src := fmt.Sprintf("fn main() i32 { let x f64 = %s; let s str = `n={x}`; if s == %q { return 0; } return 1; }", tc.lit, want)
			emitAndRun(t, src, false, 0, false)
		})
	}
}

func TestEmitInterpolatedStringFloatAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a float value part used as a call argument
	// for a str parameter — `takes(`ok={x}`)` — must materialize into a
	// PebbleStr value that flows through the call to the callee. The
	// large-exponent cases are generated from a Go fmt.Sprintf reference, as
	// in the local-declaration test above.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"f64 arg", "fn takes(s str) i32 { if s == \"ok=3.500000\" { return 1; } return 0; }\nfn main() i32 { let x f64 = 3.5; return takes(`ok={x}`); }", 1},
		{"negative f64 arg", "fn takes(s str) i32 { if s == \"ok=-2.250000\" { return 1; } return 0; }\nfn main() i32 { let x f64 = -2.25; return takes(`ok={x}`); }", 1},
		{"zero arg", "fn takes(s str) i32 { if s == \"ok=0.000000\" { return 1; } return 0; }\nfn main() i32 { let x f64 = 0.0; return takes(`ok={x}`); }", 1},
		{"f32 arg", "fn takes(s str) i32 { if s == \"ok=0.500000\" { return 1; } return 0; }\nfn main() i32 { let x f32 = 0.5; return takes(`ok={x}`); }", 1},
		{"very small arg", "fn takes(s str) i32 { if s == \"ok=0.000000\" { return 1; } return 0; }\nfn main() i32 { let x f64 = 1.0e-10; return takes(`ok={x}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
	for _, tc := range []struct {
		name string
		lit  string
		v    float64
	}{
		{"very large f64 arg", "1.0e300", 1.0e300},
		{"very large negative f64 arg", "-1.0e300", -1.0e300},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			want := "ok=" + fmt.Sprintf("%f", tc.v)
			src := fmt.Sprintf("fn takes(s str) i32 { if s == %q { return 1; } return 0; }\nfn main() i32 { let x f64 = %s; return takes(`ok={x}`); }", want, tc.lit)
			emitAndRun(t, src, false, 1, false)
		})
	}
}

func TestEmitInterpolatedStringFloatAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a float value part used as a tail-position
	// return value from a str-returning helper — `fn make(x f64) str { return
	// \`val={x}\`; }` — must materialize into a PebbleStr that is returned to
	// the caller. The large-exponent cases are generated from a Go fmt.Sprintf
	// reference, as in the local-declaration test above.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"f64 return", "fn make(x f64) str { return `val={x}`; }\nfn main() i32 { let s str = make(-3.5); if s == \"val=-3.500000\" { return 0; } return 1; }", 0},
		{"f32 return", "fn make(x f32) str { return `val={x}`; }\nfn main() i32 { let s str = make(1.25); if s == \"val=1.250000\" { return 0; } return 1; }", 0},
		{"zero return", "fn make(x f64) str { return `val={x}`; }\nfn main() i32 { let s str = make(0.0); if s == \"val=0.000000\" { return 0; } return 1; }", 0},
		{"very small return", "fn make(x f64) str { return `val={x}`; }\nfn main() i32 { let s str = make(1.0e-10); if s == \"val=0.000000\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
	for _, tc := range []struct {
		name string
		lit  string
		v    float64
	}{
		{"very large f64 return", "1.0e300", 1.0e300},
		{"very large negative f64 return", "-1.0e300", -1.0e300},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			want := "val=" + fmt.Sprintf("%f", tc.v)
			src := fmt.Sprintf("fn make(x f64) str { return `val={x}`; }\nfn main() i32 { let s str = make(%s); if s == %q { return 0; } return 1; }", tc.lit, want)
			emitAndRun(t, src, false, 0, false)
		})
	}
}

func TestEmitInterpolatedStringFloatPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with float value parts used directly as a print
	// operand must fold into the combined printf path exactly as a bare float
	// print operand does, formatting each float with %f (default precision, 6
	// decimal digits). The combined-print cases print an interpolation and
	// the same float bare in ONE print statement, so the two paths' text can
	// be compared byte-for-byte in the captured output — proving an
	// interpolated float and a directly-printed float render identically. The
	// large-exponent cases are generated from a Go fmt.Sprintf reference, as
	// in the local-declaration test above.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print positive f64", "fn main() i32 { let x f64 = 3.5; print `v={x}`; return 0; }", "v=3.500000\n"},
		{"print negative f64", "fn main() i32 { let x f64 = -2.25; print `v={x}`; return 0; }", "v=-2.250000\n"},
		{"print zero", "fn main() i32 { let x f64 = 0.0; print `v={x}`; return 0; }", "v=0.000000\n"},
		{"print f32", "fn main() i32 { let x f32 = 0.5; print `v={x}`; return 0; }", "v=0.500000\n"},
		{"print very small f64", "fn main() i32 { let x f64 = 1.0e-10; print `v={x}`; return 0; }", "v=0.000000\n"},
		{"print multiple floats with text", "fn main() i32 { let a f64 = 1.25; let b f32 = -3.5; print `{a} and {b}`; return 0; }", "1.250000 and -3.500000\n"},
		{"interpolated matches bare print", "fn main() i32 { let x f64 = 3.5; print `v={x}`, x; return 0; }", "v=3.5000003.500000\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("compiled program output = %q, want %q", out, tc.want)
			}
		})
	}
	for _, tc := range []struct {
		name string
		lit  string
		v    float64
	}{
		{"print very large f64", "1.0e300", 1.0e300},
		{"print very large negative f64", "-1.0e300", -1.0e300},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			src := fmt.Sprintf("fn main() i32 { let x f64 = %s; print `v={x}`, x; return 0; }", tc.lit)
			want := "v=" + fmt.Sprintf("%f", tc.v) + fmt.Sprintf("%f", tc.v) + "\n"
			out := emitAndRunCapture(t, src, false, 0, false)
			if out != want {
				t.Fatalf("compiled program output = %q, want %q", out, want)
			}
		})
	}
}

func TestEmitInterpolatedStringWithStrPartsAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with str value parts interleaved with text used
	// as a str-typed local's declaration initializer. Each str part is copied
	// directly (no formatting) and concatenated with surrounding text parts
	// into a single PebbleStr. We verify by comparing the local against a
	// plain string literal.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"str true", "fn main() i32 { let s str = \"world\"; let t str = `hello {s}`; if t == \"hello world\" { return 0; } return 1; }", 0},
		{"str false", "fn main() i32 { let s str = \"world\"; let t str = `goodbye {s}`; if t == \"goodbye world\" { return 0; } return 1; }", 0},
		{"empty str", "fn main() i32 { let s str = \"\"; let t str = `prefix={s}suffix`; if t == \"prefix=suffix\" { return 0; } return 1; }", 0},
		{"multiple strs", "fn main() i32 { let a str = \"foo\"; let b str = \"bar\"; let t str = `{a}-{b}`; if t == \"foo-bar\" { return 0; } return 1; }", 0},
		{"str with surrounding text", "fn main() i32 { let s str = \"rusty\"; let t str = `lang={s}`; if t == \"lang=rusty\" { return 0; } return 1; }", 0},
		{"str expression", "fn main() i32 { let s str = \"test\"; let t str = `result={s}`; if t == \"result=test\" { return 0; } return 1; }", 0},
		{"str mixed with bool", "fn main() i32 { let name str = \"pebble\"; let b bool = true; let t str = `{name}:{b}`; if t == \"pebble:true\" { return 0; } return 1; }", 0},
		{"str mixed with int", "fn main() i32 { let name str = \"version\"; let n int = 42; let t str = `{name}={n}`; if t == \"version=42\" { return 0; } return 1; }", 0},
		{"str mixed with float", "fn main() i32 { let name str = \"price\"; let p f64 = 9.99; let t str = `{name}={p}`; if t == \"price=9.990000\" { return 0; } return 1; }", 0},
		{"str mixed with all kinds", "fn main() i32 { let lang str = \"pebble\"; let ok bool = true; let ver int = 3; let pi f64 = 3.14; let t str = `{lang},{ok},{ver},{pi}`; if t == \"pebble,true,3,3.140000\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStrAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a str value part used as a call argument
	// for a str parameter — `takes(`ok={s}`)` — must materialize into a
	// PebbleStr value that flows through the call to the callee.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"str arg", "fn takes(s str) i32 { if s == \"hello=world\" { return 1; } return 0; }\nfn main() i32 { let w str = \"world\"; return takes(`hello={w}`); }", 1},
		{"empty str arg", "fn takes(s str) i32 { if s == \"x=\" { return 1; } return 0; }\nfn main() i32 { let e str = \"\"; return takes(`x={e}`); }", 1},
		{"multiple str args", "fn takes(s str) i32 { if s == \"foo-bar-baz\" { return 1; } return 0; }\nfn main() i32 { let a str = \"foo\"; let b str = \"bar\"; let c str = \"baz\"; return takes(`{a}-{b}-{c}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStrAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a str value part used as a tail-position
	// return value from a str-returning helper — `fn make(s str) str { return
	// \`val={s}\`; }` — must materialize into a PebbleStr that is returned to
	// the caller.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"str return", "fn make(s str) str { return `greet={s}`; }\nfn main() i32 { let s str = \"hi\"; let t str = make(s); if t == \"greet=hi\" { return 0; } return 1; }", 0},
		{"empty str return", "fn make(s str) str { return `tag={s}`; }\nfn main() i32 { let s str = \"\"; let t str = make(s); if t == \"tag=\" { return 0; } return 1; }", 0},
		{"multiple str returns", "fn make(a str, b str) str { return `{a}:{b}`; }\nfn main() i32 { let x str = \"one\"; let y str = \"two\"; let t str = make(x, y); if t == \"one:two\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStrInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a str value part used directly in a
	// comparison expression — `if `prefix={s}` == "prefix=hello" { ... }` —
	// must materialize into a PebbleStr that participates in
	// pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "fn main() i32 { let s str = \"hello\"; if `prefix={s}` == \"prefix=hello\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "fn main() i32 { let s str = \"hello\"; if `prefix={s}` == \"prefix=world\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStrReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string with a str
	// value part — `var s str = "initial"; s = `new={t}`;` — must materialize
	// the interpolated string into a PebbleStr and store it into the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign with str", "fn main() i32 { var s str = \"old\"; let t str = \"new\"; s = `v={t}`; if s == \"v=new\" { return 0; } return 1; }", 0},
		{"reassign with empty str", "fn main() i32 { var s str = \"old\"; let t str = \"\"; s = `v={t}`; if s == \"v=\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStrPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with str value parts used directly as a print
	// operand must materialize the whole interpolation into a temp PebbleStr
	// and print it as %s. The combined-print cases print an interpolation and
	// the same str bare in ONE print statement, so the two paths' text can be
	// compared byte-for-byte in the captured output — proving an interpolated
	// str and a directly-printed str render identically.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print str", "fn main() i32 { let s str = \"world\"; print `hello {s}`; return 0; }", "hello world\n"},
		{"print empty str", "fn main() i32 { let s str = \"\"; print `prefix={s}suffix`; return 0; }", "prefix=suffix\n"},
		{"print multiple strs", "fn main() i32 { let a str = \"foo\"; let b str = \"bar\"; print `{a}-{b}`; return 0; }", "foo-bar\n"},
		{"print str with bool", "fn main() i32 { let s str = \"yes\"; let b bool = true; print `{s}={b}`; return 0; }", "yes=true\n"},
		{"print str with int", "fn main() i32 { let s str = \"count\"; let n int = 7; print `{s}={n}`; return 0; }", "count=7\n"},
		{"print str with float", "fn main() i32 { let s str = \"val\"; let f f64 = 2.5; print `{s}={f}`; return 0; }", "val=2.500000\n"},
		{"interpolated matches bare print", "fn main() i32 { let s str = \"hello\"; print `msg={s}`, s; return 0; }", "msg=hellohello\n"},
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

func TestEmitInterpolatedStringWithCharPartsAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with char value parts interleaved with text used
	// as a str-typed local's declaration initializer. Each char part must be
	// encoded to its UTF-8 byte sequence (ASCII chars to a single byte, a
	// non-ASCII char like 'é' to its 2-byte sequence) and concatenated with
	// surrounding text parts into a single PebbleStr.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"ascii char", "fn main() i32 { let c char = 'x'; let s str = `c={c}`; if s == \"c=x\" { return 0; } return 1; }", 0},
		{"non-ascii char", "fn main() i32 { let c char = '\u00E9'; let s str = `c={c}`; if s == \"c=\u00E9\" { return 0; } return 1; }", 0},
		{"char with surrounding text", "fn main() i32 { let c char = 'x'; let s str = `before {c} after`; if s == \"before x after\" { return 0; } return 1; }", 0},
		{"multiple char parts", "fn main() i32 { let a char = 'a'; let b char = '\u00E9'; let s str = `{a}-{b}`; if s == \"a-\u00E9\" { return 0; } return 1; }", 0},
		{"char mixed with bool", "fn main() i32 { let c char = 'x'; let b bool = true; let s str = `{c}:{b}`; if s == \"x:true\" { return 0; } return 1; }", 0},
		{"char mixed with int", "fn main() i32 { let c char = 'x'; let n int = 42; let s str = `{c}={n}`; if s == \"x=42\" { return 0; } return 1; }", 0},
		{"char mixed with float", "fn main() i32 { let c char = 'x'; let p f64 = 9.99; let s str = `{c}={p}`; if s == \"x=9.990000\" { return 0; } return 1; }", 0},
		{"char mixed with str", "fn main() i32 { let c char = 'x'; let w str = \"world\"; let s str = `{c}-{w}`; if s == \"x-world\" { return 0; } return 1; }", 0},
		{"char mixed with all kinds", "fn main() i32 { let c char = '\u00E9'; let w str = \"pebble\"; let ok bool = true; let ver int = 3; let pi f64 = 3.14; let s str = `{c},{w},{ok},{ver},{pi}`; if s == \"\u00E9,pebble,true,3,3.140000\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringCharAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a char value part used as a call argument
	// for a str parameter — `takes(`ok={c}`)` — must materialize into a
	// PebbleStr value that flows through the call to the callee.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"ascii char arg", "fn takes(s str) i32 { if s == \"ok=x\" { return 1; } return 0; }\nfn main() i32 { let c char = 'x'; return takes(`ok={c}`); }", 1},
		{"non-ascii char arg", "fn takes(s str) i32 { if s == \"ok=\u00E9\" { return 1; } return 0; }\nfn main() i32 { let c char = '\u00E9'; return takes(`ok={c}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringCharAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a char value part used as a tail-position
	// return value from a str-returning helper — `fn make(c char) str { return
	// \`val={c}\`; }` — must materialize into a PebbleStr that is returned to
	// the caller.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"ascii char return", "fn make(c char) str { return `val={c}`; }\nfn main() i32 { let s str = make('x'); if s == \"val=x\" { return 0; } return 1; }", 0},
		{"non-ascii char return", "fn make(c char) str { return `val={c}`; }\nfn main() i32 { let s str = make('\u00E9'); if s == \"val=\u00E9\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringCharInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a char value part used directly in a
	// comparison expression — `if `prefix={c}` == "prefix=x" { ... }` — must
	// materialize into a PebbleStr that participates in pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "fn main() i32 { let c char = 'x'; if `prefix={c}` == \"prefix=x\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "fn main() i32 { let c char = 'x'; if `prefix={c}` == \"prefix=\u00E9\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringCharReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string with a char
	// value part — `var s str = "initial"; s = `new={c}`;` — must materialize
	// the interpolated string into a PebbleStr and store it into the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign with char", "fn main() i32 { var s str = \"old\"; let c char = 'x'; s = `v={c}`; if s == \"v=x\" { return 0; } return 1; }", 0},
		{"reassign with non-ascii char", "fn main() i32 { var s str = \"old\"; let c char = '\u00E9'; s = `v={c}`; if s == \"v=\u00E9\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringCharPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with char value parts used directly as a print
	// operand must render each char as its UTF-8 encoding. The combined-print
	// cases print an interpolation and the same char bare in ONE print
	// statement, so the two paths' text can be compared byte-for-byte in the
	// captured output — proving an interpolated char and a directly-printed
	// char render identically.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print ascii char", "fn main() i32 { let c char = 'x'; print `c={c}`; return 0; }", "c=x\n"},
		{"print non-ascii char", "fn main() i32 { let c char = '\u00E9'; print `c={c}`; return 0; }", "c=\u00E9\n"},
		{"print multiple chars", "fn main() i32 { let a char = 'a'; let b char = '\u00E9'; print `{a}-{b}`; return 0; }", "a-\u00E9\n"},
		{"print char with bool", "fn main() i32 { let c char = 'x'; let b bool = true; print `{c}={b}`; return 0; }", "x=true\n"},
		{"print char with int", "fn main() i32 { let c char = 'x'; let n int = 7; print `{c}={n}`; return 0; }", "x=7\n"},
		{"print char with float", "fn main() i32 { let c char = 'x'; let f f64 = 2.5; print `{c}={f}`; return 0; }", "x=2.500000\n"},
		{"print char with str", "fn main() i32 { let c char = 'x'; let s str = \"hi\"; print `{c}-{s}`; return 0; }", "x-hi\n"},
		{"interpolated matches bare print", "fn main() i32 { let c char = '\u00E9'; print `c={c}`, c; return 0; }", "c=\u00E9\u00E9\n"},
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

// NOTE: `print`'s C emission for str values uses fprintf(..., "%s", ...), which
// stops at the first 0x00 byte — this is a pre-existing, independent limitation
// affecting ANY str value (even non-interpolated ones containing NUL), not a bug
// introduced by the char-interpolation work. Fixing it requires switching every
// print call site to length-bounded writes like fwrite(str.data, 1, str.len, ...)
// and is explicitly out of scope for this change. All NUL-char tests below assert
// materialization correctness via .len rather than captured print output.
func TestEmitInterpolatedStringNulChar(t *testing.T) {
	t.Parallel()
	// A NUL char (Unicode scalar value 0) encoded via pebble_rt_char_to_utf8
	// produces exactly one byte: 0x00. The write pass must NOT rely on strlen
	// to determine how many bytes to copy, because strlen(0x00...) returns 0.
	// This is a regression test for the NUL-char interpolation length bug.
	for _, tc := range []struct {
		name    string
		src     string
		wantLen int // expected byte length of captured output (excluding trailing newline from print)
		check   func(t *testing.T, out string)
	}{
		{
			name:    "nul char alone",
			src:     "fn main() i32 { let c char = '\x00'; let s str = `x{c}y`; if s.len == 3 { return 0; } return 1; }",
			wantLen: 0,
			check: func(t *testing.T, out string) {
				t.Helper()
				// The program returns 0 only if the interpolated string has length 3
				// (x + NUL + y), proving the runtime correctly counted all 3 bytes.
			},
		},
		{
			name:    "nul char surrounded by text",
			src:     "fn main() i32 { let c char = '\x00'; let s str = `a{c}b`; if s.len == 3 { return 0; } return 1; }",
			wantLen: 0,
			check: func(t *testing.T, out string) {
				t.Helper()
				// The program returns 0 only if the interpolated string has length 3
				// (a + NUL + b), proving the runtime correctly counted all 3 bytes.
			},
		},
		{
			name:    "multiple nul chars",
			src:     "fn main() i32 { let a char = '\x00'; let b char = '\x00'; let s str = `x{a}y{b}z`; if s.len == 5 { return 0; } return 1; }",
			wantLen: 0,
			check: func(t *testing.T, out string) {
				t.Helper()
				// The program returns 0 only if the interpolated string has length 5
				// (x + NUL + y + NUL + z), proving the runtime correctly counted all 5 bytes.
			},
		},
		{
			name:    "nul char mixed with other types",
			src:     "fn main() i32 { let c char = '\x00'; let n int = 42; let s str = `c={c}n={n}`; if s.len == 7 { return 0; } return 1; }",
			wantLen: 0,
			check: func(t *testing.T, out string) {
				t.Helper()
				// The program returns 0 only if the interpolated string has length 7
				// (c + = + NUL + n + = + 4 + 2), proving the runtime correctly counted all 7 bytes.
			},
		},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			wantCode := 0
			if tc.wantLen != 0 {
				wantCode = 0
			}
			out := emitAndRunCapture(t, tc.src, false, wantCode, false)
			if tc.check != nil {
				tc.check(t, out)
			}
		})
	}
}

func TestEmitInterpolatedStringEnumAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with plain-enum value parts interleaved with text
	// used as a str-typed local's declaration initializer. Each enum part must
	// be formatted as its `Type.variant` name — recovered from the enum's own
	// declared source names, exactly as a bare enum print operand renders them
	// — and concatenated with surrounding text parts into a single PebbleStr.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"single enum part", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; let s str = `color={c}`; if s == \"color=Color.red\" { return 0; } return 1; }", 0},
		{"second variant", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.green; let s str = `color={c}`; if s == \"color=Color.green\" { return 0; } return 1; }", 0},
		{"third variant", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.blue; let s str = `color={c}`; if s == \"color=Color.blue\" { return 0; } return 1; }", 0},
		{"enum with surrounding text", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; let s str = `before {c} after`; if s == \"before Color.red after\" { return 0; } return 1; }", 0},
		{"multiple enum parts", "type Color = enum { red, green, blue };\nfn main() i32 { let a Color = Color.red; let b Color = Color.blue; let s str = `{a}-{b}`; if s == \"Color.red-Color.blue\" { return 0; } return 1; }", 0},
		{"three-variant enum covering two non-default cases", "type Traffic = enum { red, green, blue };\nfn main() i32 { let a Traffic = Traffic.green; let b Traffic = Traffic.blue; let s str = `{a},{b}`; if s == \"Traffic.green,Traffic.blue\" { return 0; } return 1; }", 0},
		{"enum mixed with bool", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; let b bool = true; let s str = `{c}:{b}`; if s == \"Color.red:true\" { return 0; } return 1; }", 0},
		{"enum mixed with int", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; let n int = 42; let s str = `{c}={n}`; if s == \"Color.red=42\" { return 0; } return 1; }", 0},
		{"enum mixed with float", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.green; let p f64 = 9.99; let s str = `{c}={p}`; if s == \"Color.green=9.990000\" { return 0; } return 1; }", 0},
		{"enum mixed with str", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.blue; let w str = \"world\"; let s str = `{c}-{w}`; if s == \"Color.blue-world\" { return 0; } return 1; }", 0},
		{"enum mixed with char", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; let ch char = 'x'; let s str = `{c}{ch}`; if s == \"Color.redx\" { return 0; } return 1; }", 0},
		{"enum mixed with all kinds", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.blue; let w str = \"pebble\"; let ok bool = true; let ver int = 3; let pi f64 = 3.14; let ch char = '\u00E9'; let s str = `{c},{w},{ok},{ver},{pi},{ch}`; if s == \"Color.blue,pebble,true,3,3.140000,\u00E9\" { return 0; } return 1; }", 0},
		{"inline variant literal part", "type Color = enum { red, green, blue };\nfn main() i32 { let s str = `pick={Color.green}`; if s == \"pick=Color.green\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringEnumAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a plain-enum value part used as a call
	// argument for a str parameter — `takes(`ok={c}`)` — must materialize into
	// a PebbleStr value (with the enum's switch pre-statement inside the GNU
	// statement expression the argument builds) that flows through the call to
	// the callee.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"enum arg", "type Color = enum { red, green, blue };\nfn takes(s str) i32 { if s == \"ok=Color.red\" { return 1; } return 0; }\nfn main() i32 { let c Color = Color.red; return takes(`ok={c}`); }", 1},
		{"enum arg second variant", "type Color = enum { red, green, blue };\nfn takes(s str) i32 { if s == \"ok=Color.blue\" { return 1; } return 0; }\nfn main() i32 { let c Color = Color.blue; return takes(`ok={c}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringEnumAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a plain-enum value part used as a tail-
	// position return value from a str-returning helper — `fn make(c Color)
	// str { return `val={c}`; }` — must materialize into a PebbleStr (with the
	// enum's switch pre-statement threaded into the return statement's
	// pre-statements) that is returned to the caller.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"enum return", "type Color = enum { red, green, blue };\nfn make(c Color) str { return `val={c}`; }\nfn main() i32 { let s str = make(Color.red); if s == \"val=Color.red\" { return 0; } return 1; }", 0},
		{"enum return second variant", "type Color = enum { red, green, blue };\nfn make(c Color) str { return `val={c}`; }\nfn main() i32 { let s str = make(Color.green); if s == \"val=Color.green\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringEnumInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a plain-enum value part used directly in a
	// comparison expression — `if `prefix={c}` == "prefix=Color.red" { ... }`
	// — must materialize into a PebbleStr that participates in
	// pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; if `prefix={c}` == \"prefix=Color.red\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; if `prefix={c}` == \"prefix=Color.green\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringEnumPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with plain-enum value parts used directly as a
	// print operand must render each enum as its `Type.variant` name. The
	// combined-print cases print an interpolation and the same enum bare in ONE
	// print statement, so the two paths' text can be compared byte-for-byte in
	// the captured output — proving an interpolated enum and a directly-printed
	// enum (whose output buildEnumPrintValueCalls produces from the very same
	// enumSourceName/variantSourceName helpers) render identically.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print enum", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.red; print `color={c}`; return 0; }", "color=Color.red\n"},
		{"print enum second variant", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.green; print `color={c}`; return 0; }", "color=Color.green\n"},
		{"print multiple enums", "type Color = enum { red, green, blue };\nfn main() i32 { let a Color = Color.green; let b Color = Color.blue; print `{a}-{b}`; return 0; }", "Color.green-Color.blue\n"},
		{"print enum mixed with all kinds", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.blue; let w str = \"pebble\"; let ok bool = true; let ver int = 3; let pi f64 = 3.14; let ch char = 'x'; print `{c},{w},{ok},{ver},{pi},{ch}`; return 0; }", "Color.blue,pebble,true,3,3.140000,x\n"},
		{"print inline variant literal", "type Color = enum { red, green, blue };\nfn main() i32 { print `pick={Color.blue}`; return 0; }", "pick=Color.blue\n"},
		{"interpolated matches bare print", "type Color = enum { red, green, blue };\nfn main() i32 { let c Color = Color.green; print `c={c}`, c; return 0; }", "c=Color.greenColor.green\n"},
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

func TestEmitInterpolatedStringEnumReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string with a plain-
	// enum value part — `var s str = "initial"; s = `new={c}`;` — must
	// materialize the interpolated string into a PebbleStr and store it into
	// the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign with enum", "type Color = enum { red, green, blue };\nfn main() i32 { var s str = \"old\"; let c Color = Color.red; s = `v={c}`; if s == \"v=Color.red\" { return 0; } return 1; }", 0},
		{"reassign with enum second variant", "type Color = enum { red, green, blue };\nfn main() i32 { var s str = \"old\"; let c Color = Color.blue; s = `v={c}`; if s == \"v=Color.blue\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringEnumPrintWithStructOperandCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a plain-enum value part appearing alongside a
	// composite (struct) operand switches the whole print statement to the
	// direct-sequential-fprintf path; the interpolated string must still
	// materialize (with its enum switch pre-statement) and render the enum
	// name identically.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"enum interpolation with struct operand", "type Color = enum { red, green, blue };\ntype P = struct { x i32; };\nfn main() i32 { let c Color = Color.green; let p P = P.{ x = 7 }; print `c={c}`, p; return 0; }", "c=Color.greenP{ x: 7 }\n"},
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

func TestEmitInterpolatedStringEnumTaggedUnionRejected(t *testing.T) {
	t.Parallel()
	// A tagged-union (payload-carrying enum) value part must be REJECTED with
	// a clear error, never silently interpolated as a plain enum and never a
	// crash — tagged-union interpolation is out of scope (a separate
	// follow-up). Each fixture interpolates a tagged-union value in one of the
	// positions the widening touches (a print operand, a local declaration
	// initializer, and an expression-position comparison).
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"print operand", "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.ok(42); print `r={r}`; return 0; }"},
		{"local initializer", "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.ok(42); let s str = `r={r}`; return 0; }"},
		{"comparison expression", "type Result = union enum { ok i32; error str; };\nfn main() i32 { let r = Result.ok(42); if `r={r}` == \"r=Result.ok(42)\" { return 0; } return 1; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRunRejects(t, tc.src, "tagged-union type")
		})
	}
}

func TestEmitInterpolatedStringStructAsLocalCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a struct value part used as a str-typed
	// local's declaration initializer. Each struct field must be formatted
	// as `field: value` and concatenated with surrounding text into a single
	// PebbleStr. We verify by comparing the local against a plain string
	// literal. This also exercises the F5-08 truncation regression path: a
	// struct with 2+ fields expands into MANY parts[] entries (one text label
	// + one per field + closing text), so using len(node.Parts) instead of
	// len(parts) would undercount and truncate the output.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"two int fields", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; let s str = `point={p}`; if s == \"point=Point{ x: 1, y: 2 }\" { return 0; } return 1; }", 0},
		{"three fields mixed types", "type Config = struct { name str; count int; active bool; };\nfn main() i32 { let c Config = Config.{ name = \"test\", count = 5, active = true }; let s str = `cfg={c}`; if s == \"cfg=Config{ name: test, count: 5, active: true }\" { return 0; } return 1; }", 0},
		{"float field", "type Measure = struct { val f64; };\nfn main() i32 { let m Measure = Measure.{ val = 3.14 }; let s str = `m={m}`; if s == \"m=Measure{ val: 3.140000 }\" { return 0; } return 1; }", 0},
		{"struct with surrounding text", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 10, y = 20 }; let s str = `before {p} after`; if s == \"before Point{ x: 10, y: 20 } after\" { return 0; } return 1; }", 0},
		{"struct mixed with other parts", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; let b bool = true; let n int = 99; let s str = `start_{b}_mid={p}_end_{n}`; if s == \"start_true_mid=Point{ x: 1, y: 2 }_end_99\" { return 0; } return 1; }", 0},
		{"inline struct construction", "type Point = struct { x int; y int; };\nfn main() i32 { let s str = `pt={Point.{ x = 42, y = -7 }}`; if s == \"pt=Point{ x: 42, y: -7 }\" { return 0; } return 1; }", 0},
		{"four fields all kinds", "type All = struct { a int; b bool; c f32; d str; };\nfn main() i32 { let v All = All.{ a = 1, b = false, c = 2.5, d = \"hi\" }; let s str = `v={v}`; if s == \"v=All{ a: 1, b: false, c: 2.500000, d: hi }\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStructPrint(t *testing.T) {
	t.Parallel()
	// An interpolated string with a struct value part used directly as a
	// print operand must render the struct identically to passing the same
	// struct straight to bare print. The combined-print cases print an
	// interpolation and the same struct bare in ONE print statement, so the
	// two paths' text can be compared byte-for-byte in the captured output.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"print two-field struct", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; print `pt={p}`; return 0; }", "pt=Point{ x: 1, y: 2 }\n"},
		{"print three-field struct", "type Config = struct { name str; count int; };\nfn main() i32 { let c Config = Config.{ name = \"hello\", count = 3 }; print `cfg={c}`; return 0; }", "cfg=Config{ name: hello, count: 3 }\n"},
		{"print struct with float", "type Measure = struct { val f64; };\nfn main() i32 { let m Measure = Measure.{ val = -2.5 }; print `m={m}`; return 0; }", "m=Measure{ val: -2.500000 }\n"},
		{"print inline struct construction", "type Point = struct { x int; y int; };\nfn main() i32 { print `pt={Point.{ x = 7, y = 11}}`; return 0; }", "pt=Point{ x: 7, y: 11 }\n"},
		{"interpolated matches bare print", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 5, y = 10 }; print `p={p}`, p; return 0; }", "p=Point{ x: 5, y: 10 }Point{ x: 5, y: 10 }\n"},
		{"print struct mixed with other kinds", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; let b bool = true; let n int = 42; print `mix={p},{b},{n}`; return 0; }", "mix=Point{ x: 1, y: 2 },true,42\n"},
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

func TestEmitInterpolatedStringStructAsCallArgumentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a struct value part used as a call
	// argument for a str parameter — `takes(`ok={p}`)` — must materialize
	// into a PebbleStr value that flows through the call to the callee.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"two-field struct arg", "type Point = struct { x int; y int; };\nfn takes(s str) i32 { if s == \"pt=Point{ x: 1, y: 2 }\" { return 1; } return 0; }\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; return takes(`pt={p}`); }", 1},
		{"three-field struct arg", "type Config = struct { name str; count int; };\nfn takes(s str) i32 { if s == \"cfg=Config{ name: hello, count: 3 }\" { return 1; } return 0; }\nfn main() i32 { let c Config = Config.{ name = \"hello\", count = 3 }; return takes(`cfg={c}`); }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStructAsReturnValueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a struct value part used as a tail-
	// position return value from a str-returning helper — `fn make(p Point)
	// str { return \`val={p}\`; }` — must materialize into a PebbleStr that
	// is returned to the caller.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"two-field struct return", "type Point = struct { x int; y int; };\nfn make(p Point) str { return `pt={p}`; }\nfn main() i32 { let s str = make(Point.{ x = 1, y = 2 }); if s == \"pt=Point{ x: 1, y: 2 }\" { return 0; } return 1; }", 0},
		{"three-field struct return", "type Config = struct { name str; count int; };\nfn make(c Config) str { return `cfg={c}`; }\nfn main() i32 { let s str = make(Config.{ name = \"test\", count = 7 }); if s == \"cfg=Config{ name: test, count: 7 }\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStructInComparisonCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An interpolated string with a struct value part used directly in a
	// comparison expression — `if `prefix={p}` == "prefix=Point{ ... }" { ... }`
	// — must materialize into a PebbleStr that participates in
	// pebble_rt_str_eq.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"equal after interpolation", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; if `pt={p}` == \"pt=Point{ x: 1, y: 2 }\" { return 1; } else { return 0; } }", 1},
		{"not equal after interpolation", "type Point = struct { x int; y int; };\nfn main() i32 { let p Point = Point.{ x = 1, y = 2 }; if `pt={p}` == \"pt=Point{ x: 9, y: 9 }\" { return 0; } else { return 1; } }", 1},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStructReassignmentCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A str-typed local reassigned from an interpolated string with a struct
	// value part — `var s str = "initial"; s = `new={p}`;` — must
	// materialize the interpolated string into a PebbleStr and store it into
	// the local.
	for _, tc := range []struct {
		name string
		src  string
		want int
	}{
		{"reassign with struct", "type Point = struct { x int; y int; };\nfn main() i32 { var s str = \"old\"; let p Point = Point.{ x = 3, y = 4 }; s = `v={p}`; if s == \"v=Point{ x: 3, y: 4 }\" { return 0; } return 1; }", 0},
		{"reassign with inline struct", "type Point = struct { x int; y int; };\nfn main() i32 { var s str = \"old\"; s = `v={Point.{ x = 10, y = 20}}`; if s == \"v=Point{ x: 10, y: 20 }\" { return 0; } return 1; }", 0},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRun(t, tc.src, false, tc.want, false)
		})
	}
}

func TestEmitInterpolatedStringStructNestedRejected(t *testing.T) {
	t.Parallel()
	// A struct whose field is itself a struct, tuple, array, or untagged
	// union must be cleanly REJECTED with a clear error message — only
	// scalar, str, char, and plain-enum field types are supported in
	// interpolated strings. This confirms the intentional scope boundary
	// (non-nested aggregates only) is enforced, not silently mishandled.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"nested struct field", "type Inner = struct { v int; };\ntype Outer = struct { inner Inner; };\nfn main() i32 { let o Outer = Outer.{ inner = Inner.{ v = 1 } }; let s str = `o={o}`; return 0; }"},
		{"tuple field", "type Pair = struct { pt (i32, i32); };\nfn main() i32 { let p Pair = Pair.{ pt = (1, 2) }; let s str = `p={p}`; return 0; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitRejectsWithSource(t, tc.src, "only scalar, str, char, and plain-enum field types are supported in interpolated strings")
		})
	}
}

// emitRejectsWithSource is a variant of emitAndRunRejects that passes the
// source file set to Emit. This is needed for struct-interpolation rejection
// tests because the struct-interpolation code resolves source names for
// struct declarations (via structSourceName/fieldSourceName) before checking
// whether individual field types are supported — without the file set those
// name lookups fail first.
func emitRejectsWithSource(t *testing.T, sourceText, wantSubstring string) {
	t.Helper()
	unit, snapshot, entryID, fileSet := buildFixture(t, sourceText, "main", false)
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, fileSet, nil, &buf)
	if err == nil {
		t.Fatalf("Emit succeeded for an unsupported entry shape, want rejection containing %q", wantSubstring)
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
	if !strings.Contains(err.Error(), wantSubstring) {
		t.Fatalf("Emit rejection error %q does not contain %q", err.Error(), wantSubstring)
	}
}
