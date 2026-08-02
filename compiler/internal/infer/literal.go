package infer

import (
	"fmt"
	"math/big"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

func parseIntegerLiteral(text []byte, config Config) (*big.Int, error) {
	if uint32(len(text)) > config.MaxLiteralBytes {
		return nil, fmt.Errorf("numeric literal exceeds byte limit %d", config.MaxLiteralBytes)
	}
	raw := strings.ReplaceAll(string(text), "_", "")
	if raw == "" {
		return nil, fmt.Errorf("empty integer literal")
	}
	base := 10
	digits := raw
	if strings.HasPrefix(digits, "0x") || strings.HasPrefix(digits, "0X") {
		base, digits = 16, digits[2:]
	} else if strings.HasPrefix(digits, "0b") || strings.HasPrefix(digits, "0B") {
		base, digits = 2, digits[2:]
	} else if strings.HasPrefix(digits, "0o") || strings.HasPrefix(digits, "0O") {
		base, digits = 8, digits[2:]
	}
	value, ok := new(big.Int).SetString(digits, base)
	if !ok {
		return nil, fmt.Errorf("invalid integer literal")
	}
	if uint32(value.BitLen()) > config.MaxLiteralBits {
		return nil, fmt.Errorf("integer literal exceeds bit limit %d", config.MaxLiteralBits)
	}
	return value, nil
}

func parseFloatLiteral(text []byte, config Config) (*big.Rat, error) {
	if uint32(len(text)) > config.MaxLiteralBytes {
		return nil, fmt.Errorf("numeric literal exceeds byte limit %d", config.MaxLiteralBytes)
	}
	raw := strings.ReplaceAll(string(text), "_", "")
	if raw == "" {
		return nil, fmt.Errorf("empty floating literal")
	}
	var value *big.Rat
	var err error
	if strings.HasPrefix(raw, "0x") || strings.HasPrefix(raw, "0X") || strings.ContainsAny(raw, "pP") {
		value, err = parseHexFloat(raw, config)
	} else {
		value, err = parseDecimalFloat(raw, config)
	}
	if err != nil {
		return nil, err
	}
	if uint32(value.Num().BitLen()) > config.MaxLiteralBits || uint32(value.Denom().BitLen()) > config.MaxLiteralBits {
		return nil, fmt.Errorf("floating literal exceeds bit limit %d", config.MaxLiteralBits)
	}
	return value, nil
}

func parseDecimalFloat(raw string, config Config) (*big.Rat, error) {
	sign := 1
	if strings.HasPrefix(raw, "+") {
		raw = raw[1:]
	} else if strings.HasPrefix(raw, "-") {
		sign = -1
		raw = raw[1:]
	}
	parts := strings.FieldsFunc(raw, func(r rune) bool { return r == 'e' || r == 'E' })
	if len(parts) > 2 || len(parts) == 0 {
		return nil, fmt.Errorf("invalid floating literal")
	}
	exponent := int64(0)
	if len(parts) == 2 {
		var err error
		exponent, err = parseBoundedExponent(parts[1], config)
		if err != nil {
			return nil, err
		}
	}
	sig := parts[0]
	dot := strings.IndexByte(sig, '.')
	fraction := 0
	if dot >= 0 {
		fraction = len(sig) - dot - 1
		sig = sig[:dot] + sig[dot+1:]
	}
	if sig == "" {
		return nil, fmt.Errorf("invalid floating literal")
	}
	coefficient, ok := new(big.Int).SetString(sig, 10)
	if !ok {
		return nil, fmt.Errorf("invalid floating literal")
	}
	if sign < 0 {
		coefficient.Neg(coefficient)
	}
	power := exponent - int64(fraction)
	return scaleRational(coefficient, 10, power, config)
}

func parseHexFloat(raw string, config Config) (*big.Rat, error) {
	sign := 1
	if strings.HasPrefix(raw, "+") {
		raw = raw[1:]
	} else if strings.HasPrefix(raw, "-") {
		sign = -1
		raw = raw[1:]
	}
	if !strings.HasPrefix(raw, "0x") && !strings.HasPrefix(raw, "0X") {
		return nil, fmt.Errorf("invalid hexadecimal floating literal")
	}
	raw = raw[2:]
	index := strings.IndexAny(raw, "pP")
	if index < 0 {
		return nil, fmt.Errorf("hexadecimal floating literal requires an exponent")
	}
	exponent, err := parseBoundedExponent(raw[index+1:], config)
	if err != nil {
		return nil, err
	}
	sig := raw[:index]
	dot := strings.IndexByte(sig, '.')
	fraction := 0
	if dot >= 0 {
		fraction = len(sig) - dot - 1
		sig = sig[:dot] + sig[dot+1:]
	}
	if sig == "" {
		return nil, fmt.Errorf("invalid hexadecimal floating literal")
	}
	coefficient, ok := new(big.Int).SetString(sig, 16)
	if !ok {
		return nil, fmt.Errorf("invalid hexadecimal floating literal")
	}
	if sign < 0 {
		coefficient.Neg(coefficient)
	}
	power := exponent - int64(4*fraction)
	return scaleRational(coefficient, 2, power, config)
}

func parseBoundedExponent(raw string, config Config) (int64, error) {
	if raw == "" {
		return 0, fmt.Errorf("floating literal has an empty exponent")
	}
	sign := int64(1)
	if raw[0] == '+' {
		raw = raw[1:]
	} else if raw[0] == '-' {
		sign = -1
		raw = raw[1:]
	}
	if raw == "" {
		return 0, fmt.Errorf("floating literal has an empty exponent")
	}
	var value uint64
	for _, r := range raw {
		if r < '0' || r > '9' {
			return 0, fmt.Errorf("invalid floating exponent")
		}
		digit := uint64(r - '0')
		if value > (uint64(config.MaxLiteralExponent)-digit)/10 {
			return 0, fmt.Errorf("floating exponent exceeds limit %d", config.MaxLiteralExponent)
		}
		value = value*10 + digit
	}
	if value > uint64(config.MaxLiteralExponent) {
		return 0, fmt.Errorf("floating exponent exceeds limit %d", config.MaxLiteralExponent)
	}
	return sign * int64(value), nil
}

func scaleRational(coefficient *big.Int, base int64, power int64, config Config) (*big.Rat, error) {
	if coefficient.Sign() == 0 {
		return new(big.Rat), nil
	}
	bitsPerPower := uint64(4)
	if base == 2 {
		bitsPerPower = 1
	}
	if uint64(abs64(power))*bitsPerPower+uint64(coefficient.BitLen()) > uint64(config.MaxLiteralBits)+4 {
		return nil, fmt.Errorf("floating literal exceeds bit limit %d", config.MaxLiteralBits)
	}
	factor := new(big.Int).Exp(big.NewInt(base), big.NewInt(abs64(power)), nil)
	if uint32(factor.BitLen()) > config.MaxLiteralBits {
		return nil, fmt.Errorf("floating literal exceeds bit limit %d", config.MaxLiteralBits)
	}
	if power >= 0 {
		return new(big.Rat).SetInt(new(big.Int).Mul(coefficient, factor)), nil
	}
	return new(big.Rat).SetFrac(new(big.Int).Set(coefficient), factor), nil
}

func abs64(v int64) int64 {
	if v < 0 {
		return -v
	}
	return v
}

func integerFits(value *big.Int, kind typesBuiltin, wordBits uint8) bool {
	bits, signed := kind.integerInfo(wordBits)
	if bits == 0 {
		return false
	}
	if signed {
		min := new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), uint(bits-1)))
		max := new(big.Int).Sub(new(big.Int).Lsh(big.NewInt(1), uint(bits-1)), big.NewInt(1))
		return value.Cmp(min) >= 0 && value.Cmp(max) <= 0
	}
	if value.Sign() < 0 {
		return false
	}
	max := new(big.Int).Sub(new(big.Int).Lsh(big.NewInt(1), uint(bits)), big.NewInt(1))
	return value.Cmp(max) <= 0
}

// typesBuiltin is a tiny local classifier, not a second semantic identity.
type typesBuiltin uint8

func (k typesBuiltin) integerInfo(wordBits uint8) (uint8, bool) {
	switch k {
	case builtinInt:
		return wordBits, true
	case builtinUint:
		return wordBits, false
	case builtinI8:
		return 8, true
	case builtinI16:
		return 16, true
	case builtinI32:
		return 32, true
	case builtinI64:
		return 64, true
	case builtinU8:
		return 8, false
	case builtinU16:
		return 16, false
	case builtinU32:
		return 32, false
	case builtinU64:
		return 64, false
	default:
		return 0, false
	}
}

const (
	builtinOther typesBuiltin = iota
	builtinInt
	builtinUint
	builtinI8
	builtinI16
	builtinI32
	builtinI64
	builtinU8
	builtinU16
	builtinU32
	builtinU64
	builtinF32
	builtinF64
)

func floatFits(value *big.Rat, precision int) bool {
	if value == nil {
		return false
	}
	abs := new(big.Rat).Abs(value)
	var significand uint
	var exponent uint
	if precision == 32 {
		significand = 24
		exponent = 127
	} else {
		significand = 53
		exponent = 1023
	}
	// max finite = (2^p-1) * 2^(emax-(p-1))
	maxInt := new(big.Int).Sub(new(big.Int).Lsh(big.NewInt(1), significand), big.NewInt(1))
	shift := int64(exponent) - int64(significand-1)
	max := new(big.Rat).SetInt(maxInt)
	if shift >= 0 {
		max.Mul(max, new(big.Rat).SetInt(new(big.Int).Lsh(big.NewInt(1), uint(shift))))
	} else {
		max.Quo(max, new(big.Rat).SetInt(new(big.Int).Lsh(big.NewInt(1), uint(-shift))))
	}
	return abs.Cmp(max) <= 0
}

// LiteralFitsBuiltin reports whether the exact literal serialized as
// numerator/denominator fits the given builtin kind under the target word
// width. It mirrors the Session's own literal-fitting math without requiring a
// live Session, so validation can re-check a recorded exact-literal obligation
// against a resolved concrete argument. Any parse failure, unrecognized
// literal kind, or non-numeric builtin reports false; it never panics.
func LiteralFitsBuiltin(builtin types.BuiltinKind, kind ExactLiteralKind, numerator, denominator string, wordBits uint8) bool {
	class := builtinClassKind(builtin)
	if kind == ExactInteger {
		value, ok := new(big.Int).SetString(numerator, 10)
		if !ok {
			return false
		}
		return integerFits(value, class, wordBits)
	}
	if kind == ExactFloat {
		value, ok := new(big.Rat).SetString(numerator + "/" + denominator)
		if !ok {
			return false
		}
		switch class {
		case builtinF32:
			return floatFits(value, 32)
		case builtinF64:
			return floatFits(value, 64)
		default:
			return false
		}
	}
	return false
}
