package check

import (
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/diagnostic"
	"github.com/pepplejoshua/pebble/compiler/internal/infer"
)

func validateCastFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, bool) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("06a did not produce a handoff: %+v", diagnostics.Items())
	}
	records, ok := resolveRecords(handoff, diagnostics, normalizeConfig(Config{}))
	if !ok {
		t.Fatalf("records did not resolve: %+v", diagnostics.Items())
	}
	return diagnostics, validateCastRecords(handoff, records, diagnostics, Config{})
}

// run06bFixture runs the full 06b pipeline so the new cast pass is exercised in
// its real position: it must emit C0601 (not the C0619 buildBlocks catch-all)
// and fail validation before IR construction is ever attempted.
func run06bFixture(t *testing.T, source string) (*diagnostic.DiagnosticSet, *Result) {
	t.Helper()
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte(source)})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil {
		t.Fatalf("06a failed: %+v", diagnostics.Items())
	}
	return diagnostics, run06b(handoff, diagnostics, Config{}, inputs.Types)
}

func hasCode(set *diagnostic.DiagnosticSet, code diagnostic.Code) bool {
	for _, item := range set.Items() {
		if item.Code == code {
			return true
		}
	}
	return false
}

func countCode(set *diagnostic.DiagnosticSet, code diagnostic.Code) int {
	count := 0
	for _, item := range set.Items() {
		if item.Code == code {
			count++
		}
	}
	return count
}

func TestValidateCastRecordsRejectsForbiddenStrToRawPointer(t *testing.T) {
	source := `fn f(s str) *char { return s as *char; }`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("forbidden str->*char cast was accepted")
	}
	if got := countCode(diagnostics, CodeConversion); got != 1 {
		t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("forbidden cast leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsForbiddenStrToChar(t *testing.T) {
	source := `fn f(s str) char { return s as char; }`
	diagnostics, ok := validateCastFixture(t, source)
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("forbidden str->char cast was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsForbiddenCharToStr(t *testing.T) {
	source := `fn f(c char) str { return c as str; }`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("forbidden char->str cast was accepted")
	}
	if got := countCode(diagnostics, CodeConversion); got != 1 {
		t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("forbidden cast leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsForbiddenBoolToInteger(t *testing.T) {
	source := `fn f(b bool) i32 { return b as i32; }`
	diagnostics, ok := validateCastFixture(t, source)
	if ok || !hasConversionDiagnostic(diagnostics) {
		t.Fatalf("forbidden bool->i32 cast was not rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsLegalNumericCast(t *testing.T) {
	source := `fn f(v i32) f64 { return v as f64; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal i32->f64 cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsPointerToPointerCast(t *testing.T) {
	source := `fn f(p *i32) *char { return p as *char; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("pointer-to-pointer cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsIdentityCast(t *testing.T) {
	source := `fn f(s str) str { return s as str; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("identity cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsCharToInteger(t *testing.T) {
	// The forward char -> integer direction: `c as u64` (the std/hash.peb
	// hash_char shape) must now classify compatibleExplicit and be accepted.
	source := `fn f(c char) u64 { return c as u64; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal char->u64 cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsPointerToInteger(t *testing.T) {
	// The forward pointer -> integer direction: `ptr as u64` (the
	// std/hash.peb hash_ptr shape) must now classify compatibleExplicit and be
	// accepted, mirroring the char -> integer rule exactly.
	source := `fn f(ptr *i32) u64 { return ptr as u64; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal pointer->u64 cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsPointerToUint(t *testing.T) {
	source := `fn f(ptr *i32) uint { return ptr as uint; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal pointer->uint cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsPointerToI64(t *testing.T) {
	// i64 is pointer-width-or-wider (int64_t, exactly as wide as a pointer), so
	// the forward pointer -> i64 direction stays accepted alongside u64 and uint.
	source := `fn f(ptr *i32) i64 { return ptr as i64; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal pointer->i64 cast was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsNarrowPointerToInteger(t *testing.T) {
	// A pointer cast to any integer destination narrower than the pointer
	// (u8/u16/u32/i8/i16/i32/int, each a 32-bit-or-narrower C type) must be a
	// clean C0601 checker rejection BEFORE IR construction — the backend's plain
	// (destType)(ptr) C cast for such a pair fails the mandated
	// -Wall -Wextra -Werror build with -Wpointer-to-int-cast, so the checker
	// refuses it up front. Regression guards for the exact repro shapes: no
	// silent acceptance, no C0619 internal-error leak.
	for _, tc := range []struct {
		name string
		src  string
	}{
		{"u8", "fn f(ptr *i32) u8 { return ptr as u8; }"},
		{"u16", "fn f(ptr *i32) u16 { return ptr as u16; }"},
		{"u32", "fn f(ptr *i32) u32 { return ptr as u32; }"},
		{"i8", "fn f(ptr *i32) i8 { return ptr as i8; }"},
		{"i16", "fn f(ptr *i32) i16 { return ptr as i16; }"},
		{"i32", "fn f(ptr *i32) i32 { return ptr as i32; }"},
		{"int", "fn f(ptr *i32) int { return ptr as int; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, result := run06bFixture(t, tc.src)
			if result.Successful() {
				t.Fatalf("narrow pointer->%s cast was accepted", tc.name)
			}
			if got := countCode(diagnostics, CodeConversion); got != 1 {
				t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
			}
			if hasCode(diagnostics, CodeGeneration) {
				t.Fatalf("narrow pointer->%s cast leaked the C0619 internal-error path: %+v", tc.name, diagnostics.Items())
			}
		})
	}
}

func TestValidateCastRecordsRejectsIntegerToPointer(t *testing.T) {
	// The reverse pointer direction, integer -> pointer, is deliberately out
	// of scope and must STILL be rejected with a clean C0601 — an arbitrary
	// integer is not necessarily a valid pointer. Regression guard proving the
	// new rule did not accidentally open it up.
	source := `fn f(v u64) *i32 { return v as *i32; }`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("forbidden u64->*i32 cast was accepted")
	}
	if got := countCode(diagnostics, CodeConversion); got != 1 {
		t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("forbidden cast leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsUintToPointer(t *testing.T) {
	source := `fn f(v uint) *i32 { return v as *i32; }`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("forbidden uint->*i32 cast was accepted")
	}
	if got := countCode(diagnostics, CodeConversion); got != 1 {
		t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("forbidden cast leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsIntegerToChar(t *testing.T) {
	// The reverse integer -> char direction is deliberately out of scope and
	// must STILL be rejected with a clean C0601 — an arbitrary integer is not
	// necessarily a valid Unicode scalar (e.g. the surrogate range), so this
	// direction needs its own validity-checked design later. Regression guard
	// proving the new rule did not accidentally open it.
	source := `fn f(v u32) char { return v as char; }`
	diagnostics, result := run06bFixture(t, source)
	if result.Successful() {
		t.Fatal("forbidden u32->char cast was accepted")
	}
	if got := countCode(diagnostics, CodeConversion); got != 1 {
		t.Fatalf("expected exactly one C0601, got %d: %+v", got, diagnostics.Items())
	}
	if hasCode(diagnostics, CodeGeneration) {
		t.Fatalf("forbidden cast leaked the C0619 internal-error path: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsSkipsUnresolvedRoots(t *testing.T) {
	inputs, diagnostics := factInputs(t, checkProvider{"main.peb": []byte("fn main() void {}\n")})
	handoff := run06a(inputs, diagnostics, Config{})
	if handoff == nil || handoff.Semantics == nil || handoff.Solution == nil {
		t.Fatalf("run06a failed: %+v", diagnostics.Items())
	}
	builtins := inputs.Types.Builtins()
	handoff.Records = frozenRecords{values: []retainedRecord{{
		Header: recordHeader{ID: 1, Owner: 1},
		Cast:   &castRecord{Header: recordHeader{ID: 1, Owner: 1}, Source: 1, Destination: 2, Result: 3},
	}}}
	for _, roots := range []map[valueID]infer.TypeResult{
		{1: {State: infer.TypeError}, 2: {State: infer.TypeFinal, Type: builtins.Char}},
		{1: {State: infer.TypeFinal, Type: builtins.Str}},
	} {
		fresh := diagnostic.NewDiagnosticSet()
		if !validateCastRecords(handoff, &solvedRecords{roots: roots}, fresh, Config{}) || hasConversionDiagnostic(fresh) {
			t.Fatalf("unresolved cast was not skipped: %+v", fresh.Items())
		}
	}
}

func TestValidateCastRecordsSkipsInactiveRecord(t *testing.T) {
	guard := alternativeTag{Choice: 999999, Index: 7, Guarded: true}
	handoff, _ := compatibilityValidationHandoff(t, retainedRecord{
		Header: recordHeader{ID: 1, Owner: 1, Alternative: guard},
		Cast:   &castRecord{Header: recordHeader{ID: 1, Owner: 1, Alternative: guard}, Source: 1, Destination: 2, Result: 3},
	})
	builtins := handoff.Semantics.Types().Builtins()
	fresh := diagnostic.NewDiagnosticSet()
	if !validateCastRecords(handoff, &solvedRecords{roots: map[valueID]infer.TypeResult{
		1: {State: infer.TypeFinal, Type: builtins.Str}, 2: {State: infer.TypeFinal, Type: builtins.Char},
	}}, fresh, Config{}) || hasConversionDiagnostic(fresh) {
		t.Fatalf("inactive cast record was not skipped: %+v", fresh.Items())
	}
}

func TestValidateCastRecordsAcceptsCastOfGenericTypeParameterToConcrete(t *testing.T) {
	source := `fn identity[T](x T) i32 { return x as i32; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("cast of generic type parameter T to concrete i32 was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsAcceptsConcreteCastInsideGenericFunction(t *testing.T) {
	source := `fn example[T](x T) u64 { var v i32 = 5; return v as u64; }`
	diagnostics, result := run06bFixture(t, source)
	if !result.Successful() || len(diagnostics.Items()) != 0 {
		t.Fatalf("legal i32->u64 cast inside generic function body was rejected: %+v", diagnostics.Items())
	}
}

func TestValidateCastRecordsRejectsInvalidCastInsideGenericFunction(t *testing.T) {
	type testCase struct {
		name string
		src  string
	}
	for _, tc := range []testCase{
		{"bool-to-int-in-generic", "fn example[T](x T) i32 { var b bool = false; return b as i32; }"},
		{"str-to-char-in-generic", "fn example[T](x T) char { var s str = \"hello\"; return s as char; }"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			diagnostics, ok := validateCastFixture(t, tc.src)
			if ok || !hasConversionDiagnostic(diagnostics) {
				t.Fatalf("invalid cast inside generic function was not rejected: %+v", diagnostics.Items())
			}
		})
	}
}
