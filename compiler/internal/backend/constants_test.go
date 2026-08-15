package backend

import (
	"testing"
)

// TestModuleConstantValueShapes proves the backend correctly inlines a
// module-level `let NAME = <const-expr>;` at every checker-supported value
// shape and every position that accepts a constant reference. Each case
// checks a distinct combination of value shape (literal, const-referencing-
// const, unary/binary expression, enum variant, bool/char/str/float) and
// use position (return, local init, argument, comparison, array size/repeat
// count, switch case label, range-loop bound, struct field value).
//
// Narrow-width (i8/i16/u8/u32/u64-div) checked-arithmetic constant
// expressions are intentionally not covered here: their runtime support is
// exercised by TestPlainNarrowWidthArithmeticCompilesAndRuns, while this test
// focuses on module-level constant value shapes and use positions.
func TestModuleConstantValueShapes(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"int literal return", "let X = 5; fn main() int { return X; }", 5},
		{"int literal local init", "let X = 5; fn main() int { var y int = X; return y; }", 5},
		{"int literal arg", "let X = 5; fn twice(n int) int { return n * 2; } fn main() int { return twice(X); }", 10},
		{"int literal comparison", "let X = 5; fn main() int { if X < 6 { return 7; } return 0; }", 7},
		{"int literal i32 typed return", "let X i32 = 5; fn main() int { return X as int; }", 5},

		{"ref other const", "let A = 5; let B = A; fn main() int { return B; }", 5},
		{"ref other const typed", "let A i32 = 5; let B i32 = A; fn main() int { return B as int; }", 5},
		{"transitive ref", "let A = 2; let B = A + 3; fn main() int { return B; }", 5},

		{"unary minus", "let X = -5; fn main() int { return X; }", 251},
		{"unary minus typed", "let X i32 = -5; fn main() int { return X as int; }", 251},
		{"unary minus i16", "let X i16 = -5; fn main() int { return X as int; }", 251},
		{"unary bang", "let X = !true; fn main() int { if X { return 1; } return 9; }", 9},
		{"unary tilde", "let X = ~0; fn main() int { return X; }", 255},

		{"binary add", "let X = 2 + 3; fn main() int { return X; }", 5},
		{"binary mixed ops", "let X = 2 + 3 * 4; fn main() int { return X; }", 14},
		{"binary sub", "let X = 10 - 4; fn main() int { return X; }", 6},
		{"binary divmod", "let X = 10 / 3 + 10 % 3; fn main() int { return X; }", 4},
		{"binary u64 width", "let X u64 = 2 + 3; fn main() int { return X as int; }", 5},
		{"binary i64 width", "let X i64 = 2 + 3; fn main() int { return X as int; }", 5},
		{"binary shift", "let X = 1 << 4; fn main() int { return X; }", 16},
		{"binary bitwise and", "let X = 12 & 10; fn main() int { return X; }", 8},
		{"binary bitwise or", "let X = 8 | 3; fn main() int { return X; }", 11},
		{"binary bitwise xor", "let X = 12 ^ 10; fn main() int { return X; }", 6},
		{"binary eq bool", "let X = 2 == 3; fn main() int { if X { return 1; } return 7; }", 7},
		{"binary lt bool", "let X = 2 < 3; fn main() int { if X { return 7; } return 1; }", 7},
		{"binary andand", "let X = true && false; fn main() int { if X { return 1; } return 7; }", 7},
		{"binary oror", "let X = false || true; fn main() int { if X { return 7; } return 1; }", 7},

		// Constant used as a comparison operand against a runtime enum-typed
		// value. Comparing a constant against its own literal value (e.g.
		// `X == Color.green` where `X` is itself `Color.green`) inlines to a
		// C self-comparison and trips -Wtautological-compare; that is a
		// degenerate test pattern, not a realistic one, so it's avoided here.
		{"enum variant", "type Color = enum { red, green, blue }; let X = Color.green; fn main() int { var c Color = Color.green; if c == X { return 11; } return 0; }", 11},
		{"enum variant untyped", "type Color = enum { red, green, blue }; let X = Color.blue; fn main() int { var c Color = Color.blue; if c == X { return 13; } return 0; }", 13},

		{"bool const", "let X = true; fn main() int { if X { return 7; } return 0; }", 7},
		{"char const", "let X = 'a'; fn main() int { if X == 'a' { return 4; } return 0; }", 4},
		{"str const", "let X = \"hi\"; fn main() int { if X == \"hi\" { return 12; } return 0; }", 12},
		{"float const", "let X = 2.5; fn main() int { if X == 2.5 { return 3; } return 0; }", 3},

		{"array size const", "let N = 3; fn main() int { var a [N]int = [1, 2, 3]; return a[2]; }", 3},
		{"array size expr const", "let N = 2 + 1; fn main() int { var a [N]int = [1, 2, 3]; return a[2]; }", 3},
		{"array repeat count const", "let N = 3; fn main() int { var a [3]int = [7; N]; return a[0] + a[1] + a[2]; }", 21},
		{"repeat count expr const", "let N = 1 + 2; fn main() int { var a [3]int = [7; N]; return a[0] + a[1] + a[2]; }", 21},
		{"switch case label const", "let X = 5; fn main() int { switch 5 { case X: return 1; else: return 0; } }", 1},
		{"switch case expr label", "let X = 2 + 3; fn main() int { switch 5 { case X: return 1; else: return 0; } }", 1},
		{"switch case char const", "let X = 'b'; fn main() int { switch 'b' { case X: return 2; else: return 0; } }", 2},
		{"for range const", "let N = 5; fn main() int { var s int = 0; loop 0..N : i { s = s + i; } return s; }", 10},
		{"repeat as helper arg", "let X = 3; fn twice(n int) int { return n * 2; } fn main() int { return twice(X); }", 6},
		{"const in struct field value", "let X = 5; type S = struct { f int; }; fn main() int { var s S = S.{ f = X }; return s.f; }", 5},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			emitAndRun(t, c.source, false, c.want, false)
		})
	}
}

// TestPlainNarrowWidthArithmeticCompilesAndRuns proves that i8/i16/u8/u32
// checked arithmetic and u64 division compile and produce the expected result
// for plain, non-constant operands. This positive coverage supersedes the
// former rejection test now that dedicated checked-arithmetic runtime helpers
// for these widths have landed in afb8c77 and ab67de5.
func TestPlainNarrowWidthArithmeticCompilesAndRuns(t *testing.T) {
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"plain local i16 add", "fn main() int { var y i16 = 2 + 3; return y as int; }", 5},
		{"plain local u8 add", "fn main() int { var y u8 = 2 + 3; return y as int; }", 5},
		{"plain local i8 mul", "fn main() int { var y i8 = 2 * 3; return y as int; }", 6},
		{"plain local u32 add", "fn main() int { var y u32 = 2 + 3; return y as int; }", 5},
		{"plain local u64 div", "fn main() int { var y u64 = 10 / 3; return y as int; }", 3},
	}
	for _, c := range cases {
		t.Run(c.name, func(t *testing.T) {
			emitAndRun(t, c.source, false, c.want, false)
		})
	}
}
