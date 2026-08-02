package infer

import (
	"math/big"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// powerOfTwo returns the decimal string for 2^bits, the shape the literal
// payload serializer produces via big.Int.String().
func powerOfTwo(bits uint) string {
	return new(big.Int).Lsh(big.NewInt(1), bits).String()
}

func TestLiteralFitsBuiltin(t *testing.T) {
	tests := []struct {
		name        string
		builtin     types.BuiltinKind
		kind        ExactLiteralKind
		numerator   string
		denominator string
		wordBits    uint8
		want        bool
	}{
		// i8 boundaries.
		{"i8 max", types.I8, ExactInteger, "127", "", 64, true},
		{"i8 over max", types.I8, ExactInteger, "128", "", 64, false},
		{"i8 min", types.I8, ExactInteger, "-128", "", 64, true},
		{"i8 under min", types.I8, ExactInteger, "-129", "", 64, false},
		{"i8 zero", types.I8, ExactInteger, "0", "", 64, true},

		// int depends on the target word width.
		{"int32 max", types.Int, ExactInteger, "2147483647", "", 32, true},
		{"int32 over max", types.Int, ExactInteger, "2147483648", "", 32, false},
		{"int32 min", types.Int, ExactInteger, "-2147483648", "", 32, true},
		{"int32 under min", types.Int, ExactInteger, "-2147483649", "", 32, false},
		{"int64 max", types.Int, ExactInteger, "9223372036854775807", "", 64, true},
		{"int64 over max", types.Int, ExactInteger, "9223372036854775808", "", 64, false},
		{"int32 value at 64 bits", types.Int, ExactInteger, "2147483648", "", 64, true},

		// uint depends on the target word width.
		{"uint32 max", types.Uint, ExactInteger, "4294967295", "", 32, true},
		{"uint32 over max", types.Uint, ExactInteger, "4294967296", "", 32, false},
		{"uint32 negative", types.Uint, ExactInteger, "-1", "", 32, false},
		{"uint64 max", types.Uint, ExactInteger, "18446744073709551615", "", 64, true},
		{"uint64 over max", types.Uint, ExactInteger, "18446744073709551616", "", 64, false},

		// Float literals against f32/f64.
		{"f32 one", types.F32, ExactFloat, "1", "1", 64, true},
		{"f64 one", types.F64, ExactFloat, "1", "1", 64, true},
		{"f32 2^127 fits", types.F32, ExactFloat, powerOfTwo(127), "1", 64, true},
		{"f32 2^128 overflows", types.F32, ExactFloat, powerOfTwo(128), "1", 64, false},
		{"f64 2^1023 fits", types.F64, ExactFloat, powerOfTwo(1023), "1", 64, true},
		{"f64 2^1024 overflows", types.F64, ExactFloat, powerOfTwo(1024), "1", 64, false},
		{"f32 denominator scales down", types.F32, ExactFloat, powerOfTwo(1024), powerOfTwo(1024), 64, true},

		// Category mismatches and non-numeric builtins never fit.
		{"integer against float", types.F32, ExactInteger, "1", "", 64, false},
		{"float against integer", types.I8, ExactFloat, "1", "1", 64, false},
		{"bool against integer", types.Bool, ExactInteger, "1", "", 64, false},
		{"unrecognized builtin", types.BuiltinKind(255), ExactInteger, "1", "", 64, false},

		// Malformed payloads never panic and never fit.
		{"integer nondigit", types.I8, ExactInteger, "not-a-number", "", 64, false},
		{"float nondigit numerator", types.F32, ExactFloat, "not-a-number", "1", 64, false},
		{"float nondigit denominator", types.F32, ExactFloat, "1", "not-a-number", 64, false},
		{"float empty denominator", types.F32, ExactFloat, "1", "", 64, false},
		{"unknown literal kind", types.I8, ExactLiteralKind(0), "1", "", 64, false},
	}
	for _, test := range tests {
		t.Run(test.name, func(t *testing.T) {
			got := LiteralFitsBuiltin(test.builtin, test.kind, test.numerator, test.denominator, test.wordBits)
			if got != test.want {
				t.Fatalf("LiteralFitsBuiltin(%d, %d, %q, %q, %d) = %v, want %v",
					test.builtin, test.kind, test.numerator, test.denominator, test.wordBits, got, test.want)
			}
		})
	}
}
