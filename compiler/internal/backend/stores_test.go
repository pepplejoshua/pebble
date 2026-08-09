package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitReassignUsingOwnValueCompilesAndRuns(t *testing.T) {
	// Each reassignment reads the local's own prior value: x starts at 1,
	// x = x + 1 makes it 2, the second x = x + 1 makes it 3, and the return
	// reads the final value — two increments, exit code 3. This exercises
	// that a Store's value expression can reference the very symbol being
	// reassigned (the read happens before the write).
	emitAndRun(t, "fn main() i32 { var x i32 = 1; x = x + 1; x = x + 1; return x; }", false, 3, false)
}

func TestEmitReassignInIfArmCompilesAndRuns(t *testing.T) {
	// A var declared before the if is reassigned inside the then-arm and read
	// by that same arm's return: x starts at 1, x < 10 is true, x = x + 5
	// makes it 6, exit code 6. The else arm reads the un-reassigned x (1).
	// This proves a Store is valid inside a nested block against a local
	// declared in an enclosing block — the scope map threaded through
	// buildBlock already contains x.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; if x < 10 { x = x + 5; return x; } else { return x; } }", false, 6, false)
}

func TestEmitReassignOverflowStillAborts(t *testing.T) {
	// 2147483647 + 1 overflows i32. The overflow must survive through a
	// reassignment, not just a local's initializer or a return expression:
	// x = x + 1 lowers to pebble_local_<x> = pebble_rt_checked_add_i32(
	// pebble_local_<x>, 1), which must panic through pebble_rt_panic in
	// PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not exit 0
	// and not return any specific arithmetic value.
	emitAndRun(t, "fn main() i32 { var x i32 = 2147483647; x = x + 1; return x; }", false, 0, true)
}

func TestEmitCompoundAddCompilesAndRuns(t *testing.T) {
	// The minimal repro: a compound assignment as an ordinary leading
	// statement. i += 1 lowers through the checked-arithmetic runtime helper
	// (pebble_rt_checked_add_i32), never a raw C `+=`, so i goes 5 -> 6 and the
	// process exits with the combined value 6.
	emitAndRun(t, "fn main() i32 { var i i32 = 5; i += 1; return i; }", false, 6, false)
}

func TestEmitCompoundAllOperatorsCompileAndRun(t *testing.T) {
	// Every compound operator in the language's set — +=, -=, *=, /=, %= — must
	// combine exactly like the corresponding x = x <op> y, since each lowers
	// through the same checked-arithmetic runtime helper buildExpr's
	// CheckedArithmetic case uses. Each case's expected value is independently
	// computed, not copied from the emission.
	cases := []struct {
		name   string
		source string
		want   int
	}{
		{"add", "fn main() i32 { var i i32 = 5; i += 4; return i; }", 9},
		{"sub", "fn main() i32 { var i i32 = 9; i -= 4; return i; }", 5},
		{"mul", "fn main() i32 { var i i32 = 3; i *= 7; return i; }", 21},
		{"div", "fn main() i32 { var i i32 = 21; i /= 3; return i; }", 7},
		{"mod", "fn main() i32 { var i i32 = 22; i %= 7; return i; }", 1},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			emitAndRun(t, tc.source, false, tc.want, false)
		})
	}
}

func TestEmitPostfixIncrementCompileAndRun(t *testing.T) {
	// A postfix i++ lowers through the SAME CompoundStore path as += (the
	// checker builds a postfix update as a CompoundStore with + and a
	// literal-one value child), so it must work everywhere a += does — here as
	// an ordinary leading statement. Two increments make i go 4 -> 6.
	emitAndRun(t, "fn main() i32 { var i i32 = 4; i++; i++; return i; }", false, 6, false)
}

func TestEmitPostfixDecrementCompileAndRun(t *testing.T) {
	// A postfix i-- is the -= twin: a CompoundStore with - and a literal-one
	// value child, emitted as pebble_rt_checked_sub_i32(pebble_local_<i>, 1),
	// making i go 6 -> 5.
	emitAndRun(t, "fn main() i32 { var i i32 = 6; i--; return i; }", false, 5, false)
}

func TestEmitCompoundOverflowStillAborts(t *testing.T) {
	// 2147483647 += 1 overflows i32. The overflow must survive through a
	// compound assignment, exactly as it does through a plain reassignment:
	// i += 1 lowers to pebble_local_<i> = pebble_rt_checked_add_i32(
	// pebble_local_<i>, 1), which must panic through pebble_rt_panic in
	// PEBBLE_RT_MODE_SAFE — the process must terminate abnormally, not exit 0
	// and not return a silently wrapped value. This is the proof that the
	// emission goes through the checked runtime helper rather than a naive
	// unchecked C `i += 1`.
	emitAndRun(t, "fn main() i32 { var i i32 = 2147483647; i += 1; return i; }", false, 0, true)
}

func TestEmitPostfixIncrementOverflowStillAborts(t *testing.T) {
	// A postfix i++ overflows identically to i += 1 (it IS i += 1 at the IR
	// level): 2147483647++ must panic through pebble_rt_checked_add_i32 in
	// PEBBLE_RT_MODE_SAFE, not silently wrap.
	emitAndRun(t, "fn main() i32 { var i i32 = 2147483647; i++; return i; }", false, 0, true)
}

func TestEmitCompoundDivideByZeroStillAborts(t *testing.T) {
	// i /= 0 must fault through pebble_rt_checked_div_i32 exactly like a plain
	// i = i / 0 — divide-by-zero is a checked-semantics property a compound
	// assignment must preserve, not a raw C `/=` which would be UB.
	emitAndRun(t, "fn main() i32 { var i i32 = 7; i /= 0; return i; }", false, 0, true)
}

func TestEmitCompoundReleaseWrapsOverflow(t *testing.T) {
	// In PEBBLE_RT_MODE_RELEASE the checked add wraps instead of panicking
	// (same helper, different mode): 2147483647 += 1 wraps i to INT32_MIN, so
	// the follow-on `if i < 0` is true and the process exits 77. A naive raw
	// C `i += 1` on INT32_MAX is undefined behavior, so this test proves the
	// wrap is happening through the runtime's own checked helper.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var i i32 = 2147483647; i += 1; if i < 0 { return 77; } else { return 0; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	binary := compileEmittedCRelease(t, buf.Bytes())
	runCompiledBinary(t, binary, 77, false, false)
}

func TestEmitCompoundIndexedPlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment to an array element — arr[i] += 5 — is a
	// CompoundStore whose place is a CheckedIndexPlace, emitted through the
	// same buildPlaceLValue machinery a plain indexed Store uses: the lvalue is
	// arr[pebble_rt_checked_index_i32(...)], read into the checked helper and
	// written back. i = 1 picks the 20 element, +5 makes it 25, returned.
	emitAndRun(t, "fn main() i32 { var arr [3]i32 = [10, 20, 30]; var i i32 = 1; arr[i] += 5; return arr[i]; }", false, 25, false)
}

func TestEmitCompoundIndexedPlaceEvaluatesIndexOnce(t *testing.T) {
	// The index call mutates count, so returning count proves whether the
	// compound store evaluates its place once (1) or twice (2).
	emitAndRun(t, "fn bump_and_get_index(p *i32) i32 { *p = *p + 1; return 0; } fn main() i32 { var count i32 = 0; var arr [1]i32 = [0]; arr[bump_and_get_index(&count)] += 1; return count; }", false, 1, false)
}

func TestEmitCompoundFieldPlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment to a struct field — c.count -= 2 — is a
	// CompoundStore whose place is a FieldPlace (a struct field of exactly the
	// entry's width), emitted through the same buildPlaceLValue machinery a
	// plain field Store uses: pebble_local_<c>.pebble_field_<count>, read into
	// the checked helper and written back. 10 - 2 = 8, returned.
	emitAndRun(t, `type Counter = struct { count i32; }; fn main() i32 { var c Counter = Counter.{ count = 10 }; c.count -= 2; return c.count; }`, false, 8, false)
}

func TestEmitCompoundInLoopBodyCompilesAndRuns(t *testing.T) {
	// A compound assignment and a postfix increment inside a loop body (a
	// fall-through statement sequence) route through buildLeadingStatement
	// exactly like a Store does: sum accumulates i via sum += i and i advances
	// via i++, so sum = 0+1+2+3+4 = 10, returned as the exit code. Bounded
	// execution in case of a miscompiled loop.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; var sum i32 = 0; while i < 5 { sum += i; i++; } return sum; }", false, 10, false)
}

func TestEmitCompoundI64OverflowStillAborts(t *testing.T) {
	// 9223372036854775807 += 1 overflows i64 and must panic through
	// pebble_rt_checked_add_i64 in PEBBLE_RT_MODE_SAFE — the i64 checked-helper
	// suffix really is selected for a compound assignment, not the i32 twin.
	emitAndRun(t, "fn main() i64 { var x i64 = 9223372036854775807; x += 1; return x; }", false, 0, true)
}

func TestEmitPostfixIncrementFloatCompilesAndRuns(t *testing.T) {
	// A postfix ++ on a float local lowers through the same CompoundStore path
	// (the checker's buildPostfixOne synthesizes a 1.0 float literal for a
	// float place), combining with the plain C operator: x goes 1.5 -> 2.5,
	// truncated to exit code 2.
	emitAndRun(t, "fn main() f64 { var x f64 = 1.5; x++; return x; }", false, 2, false)
}

func TestEmitCompoundDereferencePlaceCompilesAndRuns(t *testing.T) {
	// A compound assignment through a pointer — *p += 3 — is a CompoundStore
	// whose place is a DereferencePlace, emitted through the same
	// buildPlaceLValue machinery a plain write-through-pointer Store uses (the
	// null-checked dereference is the lvalue, read into the checked helper and
	// written back). v goes 7 -> 10, returned.
	emitAndRun(t, "fn main() i32 { var v i32 = 7; var p *i32 = &v; *p += 3; return v; }", false, 10, false)
}

func TestEmitDeferredCompoundStoreCompilesAndRuns(t *testing.T) {
	// A deferred compound assignment — defer x += 1; — routes through the same
	// buildCompoundStore a non-deferred compound assignment uses (the deferred
	// position accepts a CompoundStore exactly as it accepts a Store), so the
	// deferred statement runs just before the return and x goes 5 -> 6.
	emitAndRun(t, "fn main() i32 { var x i32 = 5; defer x += 1; return x; }", false, 6, false)
}

func TestEmitForLoopNoConditionCompoundUpdateCompilesAndRuns(t *testing.T) {
	// A lone no-condition for whose only clause is a CompoundStore is the
	// update-only shape, exactly like a lone no-condition Store: for ; ; i += 2
	// advances i by checked-add of 2 until the in-body break at i >= 3 fires at
	// i = 4, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var i i32 = 0; for ; ; i += 2 { if i >= 3 { break; } } return i; }", false, 4, false)
}

func TestEmitCompoundInsideIfArmAndSwitchCaseCompilesAndRuns(t *testing.T) {
	// A compound assignment in an if-arm and in a switch case body — the
	// fall-through statement-sequence positions b5be90d unified — routes through
	// the shared buildLeadingStatement exactly like a Store does: x starts 1,
	// the if-arm does x += 5 (x = 6), the switch case does x *= 3 (x = 18),
	// returned. Bounded execution is unnecessary (no loop) but harmless.
	emitAndRun(t, "fn main() i32 { var x i32 = 1; if x < 10 { x += 5; } switch x { case 6: x *= 3; else: x = 0; } return x; }", false, 18, false)
}

func TestEmitForLoopCompoundStoreUpdateCompilesAndRuns(t *testing.T) {
	// A compound-assignment as the for-loop update (step += 1) is now a
	// supported update shape: the for-header update clause accepts a
	// CompoundStore exactly as it accepts a Store, emitting the same
	// pebble_rt_checked_add_i32 call buildCompoundStore produces, so step
	// counts 0..2, total = 0+1+2 = 3, returned as the exit code.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var step i32 = 0; step < 3; step += 1 { total = total + step; } return total; }", false, 3, false)
}

func TestEmitForLoopIndexedCompoundUpdateEvaluatesPlacePerUpdate(t *testing.T) {
	// A non-plain compound update cannot put its declaration in the C for
	// header. The emitter declares the pointer before the loop and assigns its
	// address in the update expression, so the changing index is evaluated once
	// on the iteration where the update runs.
	emitAndRunBounded(t, "fn main() i32 { var arr [2]i32 = [0, 0]; var i i32 = 0; for ; i < 1; arr[i] += 1 { i = i + 1; } return arr[1]; }", false, 1, false)
}

func TestEmitForLoopPostfixIncrementUpdateCompilesAndRuns(t *testing.T) {
	// A postfix i++ as the for-loop update lowers through the same CompoundStore
	// path as step += 1 (the checker builds a for-update postfix as a
	// CompoundStore with + and a literal-one value child), so i counts 0..2 and
	// total = 0+1+2 = 3, returned as the exit code. Bounded execution.
	emitAndRunBounded(t, "fn main() i32 { var total i32 = 0; for var i i32 = 0; i < 3; i++ { total = total + i; } return total; }", false, 3, false)
}

func TestEmitStrReassignmentCompilesAndRuns(t *testing.T) {
	// Reassigning a str-typed local from a new string literal, the effect
	// observed indirectly (this backend has no way to return or print a str's
	// contents, so the reassignment's effect must be proven through a later
	// comparison): s starts as "hi", is reassigned to "ho", and the subsequent
	// comparisons prove the stored value actually changed — s == "ho" (the new
	// literal) is true and s != "hi" (the old literal) is true — so the
	// then-arm runs and the process exits 7. If the reassignment were a no-op
	// the stored value would still be "hi" and the else-arm would exit 3.
	emitAndRun(t, "fn main() i32 { var s str = \"hi\"; s = \"ho\"; if s != \"hi\" && s == \"ho\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReassignmentEscapedLiteralCompilesAndRuns(t *testing.T) {
	// The escaping correctness must survive a reassignment, not just a
	// declaration: reassigning from a literal whose decoded content forces C
	// escapes (newline, tab, quote, backslash, control byte), then comparing
	// against a differently-spelled literal that decodes to the same bytes.
	// If the reassignment's escaped C text were wrong the equality would fail
	// and the else-arm would exit 3 instead of 7.
	emitAndRun(t, "fn main() i32 { var s str = \"x\"; s = \"a\\n1\\tb\\\"c\\\\d\"; if s == \"a\\x0a1\\x09b\\x22c\\x5cd\" { return 7; } else { return 3; } }", false, 7, false)
}

func TestEmitStrReassignmentWritesC(t *testing.T) {
	// The emitted C for a str local reassigned from a string literal: the
	// reassignment is a whole-struct PebbleStr assignment whose inner
	// construction text is byte-identical to the declaration's from the same
	// literal — `pebble_local_<sym> = (PebbleStr){ .data = (const uint8_t *)
	// "ho", .len = 2 };` — the (PebbleStr) compound-literal cast being what
	// makes the shared brace text a valid C assignment expression. Symbol 25 is
	// the s local, confirmed against the real fixture dump.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var s str = \"hi\"; s = \"ho\"; if s != \"hi\" && s == \"ho\" { return 7; } else { return 3; } }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	for _, want := range []string{
		"PebbleStr pebble_local_27 = { .data = (const uint8_t *)\"hi\", .len = 2 };",
		"pebble_local_27 = (PebbleStr){ .data = (const uint8_t *)\"ho\", .len = 2 };",
		"    (void)pebble_local_27;",
	} {
		if !strings.Contains(out, want) {
			t.Errorf("emitted C missing %q:\n%s", want, out)
		}
	}
}

func TestEmitDeferredStoreCOutput(t *testing.T) {
	// Confirm the emitted C for a fixture: the deferred statement's text
	// appears immediately before the return, and nothing is emitted at the
	// defer statement's own position in program order.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { var x i32 = 0; defer x = x + 1; return x; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The deferred assignment must appear before the return.
	if !strings.Contains(out, "pebble_local_") {
		t.Errorf("emitted C has no local references:\n%s", out)
	}
	// The deferred statement should be an assignment, not a declaration.
	if strings.Contains(out, "defer") {
		t.Errorf("emitted C contains the word 'defer':\n%s", out)
	}
	// The return must be present.
	if !strings.Contains(out, "return") {
		t.Errorf("emitted C has no return statement:\n%s", out)
	}
	// Compile and run to confirm correctness.
	compileAndRunBounded(t, buf.Bytes(), 1, false)
}

func TestEmitU64CompoundAndPostfixCompilesAndRuns(t *testing.T) {
	// The compound-assignment gate (checkedSuffix(placeWidth) == "") now
	// admits a u64 place for the +, -, * family (this slice's add/sub/mul_u64
	// helpers): += and postfix ++ on a u64 local inside a u64-returning helper
	// must route through pebble_rt_checked_add_u64 and produce the correct
	// value, called from an int-entry main.
	emitAndRun(t, "fn f() u64 { var x u64 = 40; x += 2; x++; return x; } fn main() int { return f() as int; }", false, 43, false)
}

func TestEmitU64CompoundOverflowStillAborts(t *testing.T) {
	// u64 compound += follows the same checked-overflow contract as a plain
	// `x = x + y`: adding 1 to UINT64_MAX must panic through
	// pebble_rt_checked_add_u64 in SAFE mode, so the process terminates
	// abnormally.
	emitAndRun(t, "fn f() u64 { var x u64 = 18446744073709551615; x += 1; return x; } fn main() int { return f() as int; }", false, 0, true)
}

func TestEmitCharReassignmentCompilesAndRuns(t *testing.T) {
	// A char-typed reassignment: c is declared var 'a', reassigned to 'b',
	// then compared against 'b' — the process exits 1 only if the reassignment
	// actually changed the stored int32_t value.
	emitAndRun(t, "fn main() i32 { var c char = 'a'; c = 'b'; if c == 'b' { return 1; } else { return 0; } }", false, 1, false)
}

func TestEmitPointerReassignCompilesAndRuns(t *testing.T) {
	// var y i32 = 5; var p *i32 = &y; var z i32 = 10; p = &z; return *p;
	// Reassigning a pointer local to point at a different variable.
	emitAndRun(t, "fn main() i32 { var y i32 = 5; var p *i32 = &y; var z i32 = 10; p = &z; return *p; }", false, 10, false)
}
