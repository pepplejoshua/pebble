package backend

import (
	"bytes"
	"strings"
	"testing"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// TestEmitGenericHelperSpecializedAtNonEntryWidthCompilesAndRuns is the
// widened counterpart of the old rejection: a generic helper specialized at a
// concrete fixed-width integer that does NOT share the entry's C
// representation — identity[i64] called from an int-declared entry (i64 has no
// int32_t representation, so isCompatibleIntegerWidth is false and isWidth is
// too) — is now admitted by the fixed-width-integer parameter gate: the i64
// parameter is declared int64_t in the C signature, the body reads it at the
// i64 width, and identity(a) returns the caller's i64 value 5, which main
// casts back to int. Before the widening this was a clean rejection naming the
// found type; now it compiles and runs.
func TestEmitGenericHelperSpecializedAtNonEntryWidthCompilesAndRuns(t *testing.T) {
	t.Parallel()
	emitAndRun(t, "fn identity[T](x T) T { return x; } fn main() int { var a i64 = 5; var r i64 = identity(a); return r as int; }", false, 5, false)
}

func TestEmitRejectsIfWithoutElse(t *testing.T) {
	t.Parallel()
	// The checker itself refuses an if-only tail (C0607: non-void function can
	// fall through without returning), so this shape is hand-built through the
	// IR builder to exercise Emit's own requirement that the final if has an
	// else.
	unit, snapshot, entryID := buildIfWithoutElseUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLocalLeakingBetweenArms(t *testing.T) {
	t.Parallel()
	// A local declared inside one arm must not be visible in the sibling arm.
	// This hand-built unit makes the else-arm's return reference the
	// then-arm's local (symbol 25); real source can't produce this shape (the
	// reference would fail name resolution first), so it is constructed
	// directly through the IR builder. Emit must reject it cleanly — if the
	// locals map were shared across arms instead of copied per scope, the
	// else-arm would silently see symbol 25 and emit a reference to a
	// pebble_local_25 declared only inside the then-arm's C block.
	unit, snapshot, entryID := buildSiblingArmLocalLeakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonEmptyBody(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() void { let x i32 = 1; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnsupportedArithmeticOperator(t *testing.T) {
	t.Parallel()
	// Division and modulo are now lowered to pebble_rt_checked_div_i32 /
	// pebble_rt_checked_mod_i32, so no source-level CheckedArithmetic is
	// rejected for its operator anymore. This hand-built node carries a
	// bitwise operator (&), which the backend deliberately does not map to a
	// checked helper, so it must be a clean Emit rejection — not a guessed
	// lowering and not a panic in the Go test itself.
	unit, snapshot, entryID := buildUnsupportedArithmeticOperatorUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUndeclaredLocalReference(t *testing.T) {
	t.Parallel()
	// A local is declared in the body but the return references a name that
	// was never declared. The checker would never build this from valid
	// source (resolution fails first), so it is hand-built through the IR
	// builder to exercise Emit's own requirement that a SymbolValue reference
	// only symbols declared earlier in the entry body.
	unit, snapshot, entryID := buildUndeclaredLocalReferenceUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsStoreToUndeclaredSymbol(t *testing.T) {
	t.Parallel()
	// A Store's place must name a local declared earlier in the entry body.
	// The checker would never build a reassignment of an undeclared name from
	// valid source (resolution fails first), so it is hand-built through the
	// IR builder to exercise Emit's own in-scope requirement on the place's
	// symbol.
	unit, snapshot, entryID := buildStoreToUndeclaredSymbolUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNonI32StoreValue(t *testing.T) {
	t.Parallel()
	// A reassignment's new value must be a valid i32 expression. The checker
	// rejects a bool assigned to an i32 var itself (T0505: the types do not
	// unify), so this shape is hand-built through the IR builder to exercise
	// Emit's own i32 gate on the Store's value child via buildExpr.
	unit, snapshot, entryID := buildNonI32StoreValueUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsStoreToNonStoragePlace(t *testing.T) {
	t.Parallel()
	// A Store's place must be StoragePlace, CheckedIndexPlace, or
	// DereferencePlace. Real source never produces a TuplePlace as a Store's
	// writable target (reassigning a whole tuple element in place is not
	// supported), so this shape is hand-built through the IR builder to
	// exercise Emit's own place-kind requirement.
	unit, snapshot, entryID := buildStoreToNonStoragePlaceUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsWhileAsTail(t *testing.T) {
	t.Parallel()
	// A terminal while is only accepted as the block's tail when it is
	// exhaustive — a literal `true` condition with no break targeting the
	// loop (see terminalWhileIsExhaustive). This unit's loop is the opposite:
	// its condition is a comparison (`1 < 2`, a tir.BinaryValue), which can
	// exit normally, so a non-void body ending in it falls through and stays
	// rejected. The checker itself rejects this shape
	// from real source (C0607: non-void function can fall through without
	// returning — a while's condition evaluation does not guarantee a return),
	// so this unit is hand-built through the IR builder to exercise Emit's own
	// rejection of a non-exhaustive While as the last child of the entry body
	// block.
	unit, snapshot, entryID := buildWhileAsTailUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsWhileTrueWithBreakAsTail(t *testing.T) {
	t.Parallel()
	// A terminal while with a constant-true condition is only exhaustive when
	// its loop body never breaks out of the loop itself: a break targeting the
	// loop is the one way control can leave an otherwise-infinite loop and fall
	// through past it, which a non-void body cannot allow without a trailing
	// return. The checker itself rejects this shape from real source (C0607 —
	// the break makes the loop fall through), so this unit is hand-built
	// through the IR builder, the same pattern buildWhileAsTailUnit uses, to
	// pin the backend's own criterion that a terminal while must contain no
	// Break whose Target is the loop's Region. The loop body also carries a
	// return so the rejection is specifically about the break, not the body.
	unit, snapshot, entryID := buildWhileTrueWithBreakAsTailUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsLocalLeakingBetweenLoopIfArms(t *testing.T) {
	t.Parallel()
	// A local declared inside one arm of a loop-body if must not be visible in
	// the sibling arm. This hand-built unit makes the else-arm's Store target
	// the then-arm's local (symbol 25); real source can't produce this shape
	// (the reference would fail name resolution first), so it is constructed
	// directly through the IR builder. Emit must reject it cleanly — if the
	// locals map were shared across arms instead of copied per scope, the
	// else-arm would silently accept symbol 25 and emit a reference to a
	// pebble_local_25 declared only inside the then-arm's C block.
	unit, snapshot, entryID := buildLoopIfArmLocalLeakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsBoolLocalInIntegerPosition(t *testing.T) {
	t.Parallel()
	// A bool local referenced where an integer is expected must be rejected by
	// this backend. Real source `var flag bool = true; return flag;` never
	// reaches Emit — the checker itself rejects it (C0601: cannot convert a
	// bool for an i32 return value, confirmed against a fixture) — so the shape
	// is hand-built through the IR builder: the i32 entry declares a bool local
	// (Initialize for symbol 25 with a bool literal) and its Return references
	// that same symbol as a bool-typed SymbolValue. buildExpr's width gate must
	// reject the bool-typed value in the integer return position.
	unit, snapshot, entryID := buildBoolLocalReturnUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsNonI32ReturnValue(t *testing.T) {
	t.Parallel()
	// A bool literal is not an i32 expression. The checker would never build
	// this shape itself, so it is hand-built through the IR builder to
	// exercise Emit's own non-i32 rejection.
	unit, snapshot, entryID := buildNonI32ReturnUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsI32EmptyBody(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID := buildI32EmptyBodyUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitAcceptsArgvParameter(t *testing.T) {
	t.Parallel()
	// The single-parameter main(argv []str) entry form is now a supported
	// shape. This test used to be TestEmitRejectsParameters and pinned the old
	// behavior that ANY entry parameter was rejected; the checker has accepted
	// the []str argv form since its entry-validation audit (validArgvParameter),
	// and this backend now wires it through (see TestEntryArgvSlice /
	// TestEntryArgvVoid in emit_test.go). A void-result argv entry emits
	// cleanly; the two-parameter main(argc int, argv []str) form remains
	// intentionally unsupported and is pinned by TestEntryArgvRejectsTwoParameters.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main(args []str) void {}", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, nil, nil, &buf); err != nil {
		t.Fatalf("Emit failed for main(argv []str) void: %v", err)
	}
}

func TestEmitRejectsUnsupportedResultType(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() u32 { return 0; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnknownEntrySymbol(t *testing.T) {
	t.Parallel()
	unit, snapshot, _, _ := buildFixture(t, "fn main() void {}", "main", true)
	assertEmitRejects(t, unit, snapshot, symbol.SymbolID(0x7FFFFFFF))
}

func TestEmitRejectsBreakAsTopLevelLeadingStatement(t *testing.T) {
	t.Parallel()
	// A Break reaching the entry body outside any loop body is unreachable from
	// real source — the checker's C0611 requires a jump to have a valid
	// enclosing loop target, so a break at the function's top level never
	// survives checking — but a hand-built unit can still place one as a
	// leading statement in the entry body block. buildBlock's generic
	// unsupported-statement path (buildLeadingStatement's default) must reject
	// it cleanly exactly like any other non-leading statement kind, not
	// silently emit a break outside a loop.
	unit, snapshot, entryID := buildTopLevelBreakUnit(t)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsUnboundRangeLoop(t *testing.T) {
	t.Parallel()
	// The unbound form (`loop start..end { ... }`, no `: name`) builds a
	// RangeLoop whose Symbol field is zero, and there is no way to observe
	// such a loop's iteration count from inside the body, so it is rejected
	// cleanly rather than lowered with a synthetic counter the source never
	// names. The checker now rejects the unbound form from real source with
	// C0622 before IR construction, so this hand-builds the unit directly
	// through the IR builder to keep exercising buildRangeLoop's own
	// rangeNode.Symbol == 0 guard as defense-in-depth for hand-built TIR.
	unit, snapshot, entryID := buildUnboundRangeLoopUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "unbound range loop")
}

func TestEmitForLoopRejectsCompoundStoreInitializer(t *testing.T) {
	t.Parallel()
	// A compound-assignment as the for-loop initializer (for x += 1; ...) is
	// reachable from real source but out of scope: the initializer must be a
	// single local declaration.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var x i32 = 0; for x += 1; x < 3; { } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "for loop initializer is a CompoundStore")
}

func TestEmitForLoopRejectsExpressionStatementClause(t *testing.T) {
	t.Parallel()
	// A discarded-expression clause (for x + 1; ... or for ; ; x + 1 ...) is
	// reachable from real source but out of scope: a no-condition for whose
	// only clause is an ExpressionStatement is ambiguous (expression
	// initializer or expression update — both out of scope, so the rejection
	// is unambiguous in outcome).
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var x i32 = 0; for x + 1; ; { break; } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "with no condition has a ExpressionStatement clause")
	unit, snapshot, entryID, _ = buildFixture(t, "fn main() i32 { var x i32 = 0; for ; ; x + 1 { break; } return x; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "with no condition has a ExpressionStatement clause")
}

func TestEmitRejectsI64MainCallsI32Helper(t *testing.T) {
	t.Parallel()
	// A called function must resolve to the entry's own integer width — there
	// is no cast/coercion lowering, the same reasoning 10.13 established for
	// locals. An i64 entry calling an i32 helper is a legal, checker-accepted
	// program, so this is a genuine backend-scope rejection naming the width
	// mismatch.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return 21; } fn main() i64 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i64")
}

func TestEmitRejectsI32MainCallsI64Helper(t *testing.T) {
	t.Parallel()
	// The reverse direction: an i32 entry calling an i64 helper is likewise a
	// clean width-mismatch rejection.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i64 { return 21; } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsBareI64LocalReferenceInI32Context(t *testing.T) {
	t.Parallel()
	// Regression: a bare (uncast) reference to a mismatched-width local used
	// where the width matters must still be a clean rejection, never a silent
	// coercion. The checker accepts `return y;` (an i64 local from an
	// i32-returning function), but the backend's buildExpr width gate rejects
	// the SymbolValue's own resolved i64 type against the ambient i32,
	// naming the i32 it wanted.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var y i64 = 100; return y; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want i32")
}

func TestEmitRejectsMixedWidthFloatArithmeticAndComparison(t *testing.T) {
	t.Parallel()
	for _, source := range []string{
		"fn main() f64 { var a f32 = 1.0; var b f64 = 2.0; return a + b; }",
		"fn main() f64 { var a f32 = 1.0; var b f64 = 2.0; if a < b { return 1.0; } else { return 0.0; } }",
	} {
		if _, _, _, _, err := buildFixtureMaybeFailing(t, source, "main", false); err == nil {
			t.Fatalf("checker accepted mixed-width float expression: %s", source)
		}
	}
}

func TestEmitRejectsEntryReachedByHelperCycle(t *testing.T) {
	t.Parallel()
	// The one cycle shape still rejected: a helper calling the entry function
	// (main) back closes a cycle through the entry, which is emitted under the
	// fixed C name pebble_user_main — not as a pebble_fn_<symbolID> helper the
	// forward-declaration pass covers — so the backend cannot lower a call to
	// it and rejects the cycle cleanly rather than emit a call to an
	// undeclared C identifier.
	unit, snapshot, entryID, _ := buildFixture(t, "fn helper() i32 { return main(); } fn main() i32 { return helper(); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "recursive call through the entry function is not supported")
}

func TestEmitRejectsParameterWidthMismatch(t *testing.T) {
	t.Parallel()
	// The i64 PARAMETER of this fixture is now accepted — the fixed-width-
	// integer widening admits an i64 parameter in an i32 entry (declared
	// int64_t in the C signature) — but the PROGRAM is still genuinely
	// width-mismatched: main, declared i32, returns f(0), whose i64 result is
	// consumed where the entry's i32 is expected with no cast. That mismatch
	// is a clean rejection at the call-result width gate (buildExpr's
	// DirectCall case), naming the mismatched width, never a coercion. This is
	// the same width-consistency rule, now applied to the call's RESULT rather
	// than its parameter.
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(a i64) i64 { return 0; } fn main() i32 { return f(0); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "DirectCall of type i64, want i32")
}

func TestEmitRejectsCallArgumentCountMismatch(t *testing.T) {
	t.Parallel()
	// A call site passing fewer (or more) arguments than the callee declares
	// parameters is unreachable from real source — the checker rejects a wrong
	// argument count itself — so it is hand-built through the IR builder to
	// exercise Emit's own requirement that a DirectCall's child count matches
	// the callee's declared parameter count.
	unit, snapshot, entryID := buildCallArgumentCountMismatchUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want 2")
}

func TestEmitTupleWithStrElementCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A tuple whose element type is a scalar builtin beyond the original
	// entry-width/bool pair — here a str element — is reachable from real
	// source (the checker builds the declaration fine), and since composite
	// print slice 2's tuple-of-scalars scope this backend emits it: the tuple
	// typedef's element C type is PebbleStr and the element expression is
	// built by buildStrOperand. The program constructs the tuple and prints
	// it, exercising the whole tuple-typedef + brace-list path.
	out := emitAndRunCapture(t, "fn main() i32 { let t (i32, str) = (1, \"hi\"); print t; return 0; }", false, 0, false)
	if want := "(1, hi)\n"; out != want {
		t.Fatalf("compiled program output = %q, want %q", out, want)
	}
}

func TestEmitTupleNestedMultipleLevelsCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A tuple-only dependency chain nested more than one level deep — b embeds
	// a, c embeds b — is exactly the shape the selective depth fix now allows
	// (struct/tuple/optional-only chains may nest arbitrarily deep; only a
	// chain that passes through an array keeps the depth>1 rejection). Before
	// the fix orderAggregateTypes rejected this as "more than one level of
	// nesting" (this test was TestEmitRejectsTupleNestedMoreThanOneLevel). The
	// three tuple typedefs must be emitted dependency-first — the innermost
	// (i32, i32) before the ((i32, i32), i32) that embeds it, before the
	// (((i32, i32), i32), i32) that embeds that — and the program must compile
	// and run.
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { let a (i32, i32) = (1, 2); let b ((i32, i32), i32) = (a, 3); let c (((i32, i32), i32), i32) = (b, 4); return 1; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	var names []string
	for _, line := range strings.Split(out, "\n") {
		trimmed := strings.TrimSpace(line)
		if strings.HasPrefix(trimmed, "} pebble_tuple_") {
			names = append(names, strings.TrimSuffix(strings.TrimPrefix(trimmed, "} "), ";"))
		}
	}
	if len(names) < 3 {
		t.Fatalf("expected three nested tuple typedefs, got %v:\n%s", names, out)
	}
	inner, middle, outer := names[0], names[1], names[2]
	innerEnd := strings.Index(out, "} "+inner+";")
	middleEnd := strings.Index(out, "} "+middle+";")
	outerEnd := strings.Index(out, "} "+outer+";")
	if innerEnd < 0 || middleEnd < 0 || outerEnd < 0 {
		t.Fatalf("failed to locate the three tuple typedef definitions:\n%s", out)
	}
	if !(innerEnd < middleEnd && middleEnd < outerEnd) {
		t.Fatalf("tuple typedefs are not dependency-first (inner %d, middle %d, outer %d):\n%s", innerEnd, middleEnd, outerEnd, out)
	}
	compileAndRun(t, buf.Bytes(), 1, false)
}

func TestEmitRejectsParenWrappedAggregateArgument(t *testing.T) {
	t.Parallel()
	// The one shape in the inline-construction space that is still genuinely
	// rejected (10.25): an aggregate constructed inline but wrapped in an extra
	// set of parens — f(((1, 2))) or f((Point.{ x = 1, y = 2 })) — arrives at
	// the call site as a SourceAlias wrapping the TupleValue/RecordConstruct
	// (confirmed against a real fixture dump). This backend does not unwrap a
	// SourceAlias-wrapped argument for ANY argument type — the scalar analog
	// f((1)) is likewise rejected as "a SourceAlias of type int, want i32" —
	// so the aggregate forms stay a clean rejection naming what was found,
	// never a guessed lowering. (10.24's two rejection tests, which rejected
	// ALL inline construction as a call argument, became stale when inline
	// construction was added — the fixtures f((1, 2)) and
	// f(Point.{ x = 1, y = 2 }) are now the positive cases
	// TestEmitInlineTupleArgumentCompilesAndRuns /
	// TestEmitInlineStructArgumentCompilesAndRuns — and this test is their
	// replacement.)
	unit, snapshot, entryID, _ := buildFixture(t, "fn f(t (i32, i32)) i32 { return t.1; } fn main() i32 { return f(((1, 2))); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a reference to a tuple-typed local in scope or a tuple literal")

	unit, snapshot, entryID, _ = buildFixture(t, "type Point = struct { x i32; y i32; };\nfn f(p Point) i32 { return p.x; } fn main() i32 { return f((Point.{ x = 1, y = 2 })); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a reference to a struct-typed local in scope or a struct literal")
}

func TestEmitRejectsTupleLiteralIndex(t *testing.T) {
	t.Parallel()
	// Indexing a tuple literal directly — (1, 2).1 — is out of scope: the
	// checker lowers it to a TupleElementValue whose child is the TupleValue
	// being indexed (not a tuple-typed local) and whose element type comes out
	// as the unanchored `int` builtin (confirmed against a real fixture). The
	// tuple's int element is not the entry's width, so the tuple typedef pass
	// rejects it cleanly; this keeps the only supported element read exactly
	// the tuple-local Load(TuplePlace) shape.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x i32 = (1, 2).1; return x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsOptionalWithUnsupportedPayloadType(t *testing.T) {
	t.Parallel()
	// An optional whose payload type is neither the entry's width nor bool —
	// here a str payload — is reachable from real source (the checker builds
	// the declaration fine), so this is a genuine backend-scope rejection. The
	// optional typedef pass inspects the payload type first and rejects the
	// str field with a clear error naming the wanted types, so no C is written.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { let x ?str = some \"hi\"; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "want a fixed-width integer, bool, tuple, struct, or enum")
}

func TestEmitOptionalUnwrapOfU8PayloadCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A u8 optional payload's force-unwrap now routes to
	// pebble_rt_checked_unwrap_u8. This shape was previously a clean
	// rejection ("has no runtime unwrap helper") before that runtime helper
	// existed (this test was TestEmitRejectsOptionalUnwrapOfU8Payload); now
	// it compiles and runs, returning 5.
	emitAndRun(t, "fn main() i32 { var o ?u8 = some 5; return o! as i32; }", false, 5, false)
}

func TestEmitRejectsStructUnsupportedFieldType(t *testing.T) {
	t.Parallel()
	// A struct whose field type is neither a fixed-width integer nor a
	// supported compound type — here a char field — is reachable from real
	// source (the checker builds the declaration and construction fine), so
	// this is a genuine backend-scope rejection. The struct typedef pass
	// inspects each field's resolved type first and rejects the char field with
	// a clear error naming the wanted types, so no C is written. A str field,
	// by contrast, is now supported (TestEmitStrStructField*).
	unit, snapshot, entryID, _ := buildFixture(t, "type S = struct { c char; };\nfn main() i32 { let x S = S.{ c = 'x' }; return 1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "field type char is not supported, want a fixed-width integer, bool, str, tuple, struct, enum, pointer, slice, function type, or runtime type")
}

func TestEmitRejectsStrLoadOfNonStrField(t *testing.T) {
	t.Parallel()
	// Defense for hand-built IR: a Load whose place is a FieldPlace but whose
	// loaded type is NOT str must be a clean rejection naming the loaded type
	// (buildStrOperand is the str grammar; a real-source integer field read
	// goes through buildExpr's own Load case instead, so this mismatch can
	// only be hand-built). The crafted argument is a str-typed call-site arg
	// carrying an i32-typed Load of a FieldPlace, so buildStrOperand's new
	// Load case validates the place kind first (a FieldPlace, passes) and then
	// rejects the loaded type.
	unit, snapshot, entryID := buildStrCallCraftedArgUnit(t, buildStrLoadOfIntFieldArg)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "contains a Load of type i32, want str")
}

func TestEmitRejectsStrLoadOfNonFieldPlace(t *testing.T) {
	t.Parallel()
	// Defense for hand-built IR: a str-typed Load whose place is NOT a
	// FieldPlace (here a bare StoragePlace) must be a clean rejection naming
	// the found place kind, never a guessed lowering. This preserves the
	// existing rejection of unrelated str-shaped loads while opening only the
	// FieldPlace path — real source cannot reach this shape (str slices/arrays
	// are themselves rejected upstream), so it is hand-built.
	unit, snapshot, entryID := buildStrCallCraftedArgUnit(t, buildStrLoadOfStoragePlaceArg)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "contains a str Load whose place is a StoragePlace, want a FieldPlace")
}

func TestEmitRejectsStructFieldAssignment(t *testing.T) {
	t.Parallel()
	// FieldPlace stores lower through the same lvalue machinery as pointer and
	// indexed writes, preserving the mutation for the following read.
	emitAndRun(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { var point Point = Point.{ x = 1, y = 2 }; point.x = 5; return point.x; }", false, 5, false)
}

func TestEmitRejectsStructFieldReadOffLiteral(t *testing.T) {
	t.Parallel()
	// Reading a field directly off a struct literal (Point.{ x = 1, y = 2 }.x)
	// is reachable from real source but lowers to a FieldValue whose base is
	// the RecordConstruct itself, not a StoragePlace naming a struct local — a
	// value-category shape out of scope this slice (only Load(FieldPlace) of a
	// struct local is supported). The integer expression builder rejects the
	// FieldValue cleanly rather than guessing.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn main() i32 { return Point.{ x = 1, y = 2 }.x; }", "main", false)
	assertEmitRejects(t, unit, snapshot, entryID)
}

func TestEmitRejectsNestedStructFieldAccess(t *testing.T) {
	t.Parallel()
	// Nested field access (o.inner.x, where inner is itself a struct-typed
	// field) is reachable from real source but out of scope twice over: the
	// struct-of-struct field type is itself rejected by the typedef pass first
	// (a struct field must be the entry's width or bool), so the program is a
	// clean rejection naming the unsupported field type before the nested read
	// (a FieldPlace whose base is another FieldPlace) is even reached.
	emitAndRun(t, "type Inner = struct { x i32; };\ntype Outer = struct { inner Inner; y i32; };\nfn main() i32 { let i Inner = Inner.{ x = 7 }; let o Outer = Outer.{ inner = i, y = 8 }; return o.inner.x; }", false, 7, false)
}

func TestEmitRejectsStrReassignmentFromLocal(t *testing.T) {
	t.Parallel()
	// Reassigning a str local from another str local (s = t) is reachable from
	// real source (confirmed against a real fixture dump: the Store's value
	// child is a SymbolValue) but out of scope — this slice is deliberately
	// literal-to-literal only — so it is a clean rejection naming what was
	// found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() i32 { var s str = \"hi\"; var t str = \"ho\"; s = t; return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a SymbolValue")
}

func TestEmitRejectsStrReassignmentFromCall(t *testing.T) {
	t.Parallel()
	// Reassigning a str local from a str-returning call (s = g()) is reachable
	// from real source (confirmed against a real fixture dump: the Store's
	// value child is a DirectCall) but out of scope — literal-to-literal only —
	// so it is a clean rejection naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn g() str { return \"ho\"; } fn main() i32 { var s str = \"hi\"; s = g(); return 0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a DirectCall")
}

func TestEmitRejectsStrReassignmentFromConcat(t *testing.T) {
	t.Parallel()
	// Reassigning a str local from concatenation (s = "h" + "i") lowers to a
	// Store whose value child is a BinaryValue of type str — concatenation
	// lowers to a str-typed BinaryValue, and interpolation is the separate
	// InterpolatedString node. Since the checker now rejects `str + str`
	// (C0603) this shape is no longer reachable from real source, so it is
	// hand-built through the IR builder to keep exercising Emit's own
	// rejection — concatenation/interpolation needs runtime primitives this
	// backend has none of — as a clean rejection naming what was found.
	unit, snapshot, entryID := buildStrConcatReassignmentUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "from a BinaryValue")
}

func TestEmitRejectsNonStrCheckedIndex(t *testing.T) {
	t.Parallel()
	// Indexing an array literal directly (['h', 'i'][0]) is reachable from
	// real source and lowers to a bare CheckedIndex too — an array literal
	// has no addressable place, so it cannot form a Load(CheckedIndexPlace).
	// A str base and a slice-typed base (a call result, a slice-typed local
	// or place) are both accepted (see buildSliceIndexValue), but an
	// ARRAY-typed base still is not — there is no way to materialize an
	// array literal's value without inventing a new array-temp lowering.
	// This remains a clean rejection naming what was found (the ArrayValue
	// base and its [2]char type), never a guessed lowering; only the
	// message wording changed (from "want str" to naming the now-wider set
	// of accepted base shapes) when the slice-typed-base case was added.
	emitAndRunRejects(t, "fn main() i32 { let c char = ['h', 'i'][0]; return 0; }", "indexes a ArrayValue of type [2]char, want a slice-typed value")
}

func TestEmitRejectsTupleReturningHelperAsArgument(t *testing.T) {
	t.Parallel()
	// Calling a tuple-returning helper outside the one supported position — as
	// an argument to another function (f(makeT())) — is reachable from real
	// source: the outer DirectCall's argument is the inner DirectCall. The
	// aggregate-argument builder rejects it cleanly, naming what was found,
	// never a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn f(t (i32, i32)) i32 { return t.0 + t.1; } fn main() i32 { return f(makeT()); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "argument 0 is a DirectCall")
}

func TestEmitRejectsStructReturningHelperAsArgument(t *testing.T) {
	t.Parallel()
	// The struct side of the argument-position rejection: f(makeP()) passes a
	// struct-returning call as an argument, which the aggregate-argument
	// builder rejects naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "type Point = struct { x i32; y i32; };\nfn makeP() Point { return Point.{ x = 20, y = 22 }; } fn f(p Point) i32 { return p.x + p.y; } fn main() i32 { return f(makeP()); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "argument 0 is a DirectCall")
}

func TestEmitRejectsTupleWholeReassignmentFromCallValue(t *testing.T) {
	t.Parallel()
	// Reassigning a whole tuple-typed local from a call to a tuple-returning
	// helper (`p = make_tuple();`) is reachable from real source but out of
	// scope this slice: the supported new-value shapes are a reference to an
	// in-scope tuple-typed local or a tuple literal (a TupleValue), mirroring
	// buildAggregateArgument's tuple argument shapes and the struct
	// reassignment's own DirectCall deferral. A DirectCall value reaches
	// buildStoreCore's tuple branch and is a clean rejection naming what was
	// found, never a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "fn make_tuple() (int, int) { return (9, 10); } fn main() int { var p (int, int) = (1, 2); p = make_tuple(); return p.0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "reassigns a tuple-typed place")
}

func TestEmitRejectsTupleReturningHelperAsOperand(t *testing.T) {
	t.Parallel()
	// Calling a tuple-returning helper as an operand of an element read —
	// return makeT().0; — is reachable from real source: the read lowers to a
	// TupleElementValue whose child is the DirectCall, not a SymbolValue naming
	// a tuple-typed local. The integer expression builder rejects the
	// non-local base cleanly.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn main() i32 { return makeT().0; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "of a DirectCall")
}

func TestEmitRejectsTupleReturningHelperInAnotherHelpersReturn(t *testing.T) {
	t.Parallel()
	// Calling a tuple-returning helper as another tuple-returning helper's
	// return value — `return makeT();` from makeT2 — is reachable from real
	// source but deliberately out of scope (a call is only supported as a
	// direct-initializer use, never this return-forwarding position). The
	// tuple-returning helper's own tail Return routes through
	// buildAggregateReturnValue, which rejects the DirectCall value cleanly,
	// naming what was found.
	unit, snapshot, entryID, _ := buildFixture(t, "fn makeT() (i32, i32) { return (20, 22); } fn makeT2() (i32, i32) { return makeT(); } fn main() i32 { let t (i32, i32) = makeT2(); return t.0 + t.1; }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "returns a DirectCall")
}

func TestEmitRejectsEntryReturningTuple(t *testing.T) {
	t.Parallel()
	// The entry itself cannot declare a tuple/struct result type: its C return
	// type stays the scalar entryReturnType (integer, or a float since Float
	// Stage A) regardless of what the language
	// lets a helper write. validateEntrySignature rejects the tuple result
	// exactly as it always has, with the accepted-result list since Float
	// Stage A extended by f32 and f64.
	unit, snapshot, entryID, _ := buildFixture(t, "fn main() (i32, i32) { return (1, 2); }", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "entry function result type is (i32, i32), want void, int, i32, i64, f32, or f64")
}

func TestEmitSwitchRejectsNonExhaustiveNoElse(t *testing.T) {
	t.Parallel()
	// A switch with no else and non-exhaustive cases, used as a tail
	// statement: some paths do not end in a return. The checker may or may
	// not reject this; if it reaches the backend, the switch as a whole is
	// a tail that must guarantee a return on every path, and a non-exhaustive
	// switch without else does not — but a C switch is a valid tail only if
	// every reachable path returns. The checker is expected to reject this
	// shape (no else means some paths fall through without returning), so
	// the fixture should fail at check time.
	_, _, _, _, err := buildFixtureMaybeFailing(t, "fn main() i32 { switch 1 { case 1: return 10; } }", "main", false)
	if err == nil {
		t.Log("checker accepted non-exhaustive switch without else — this may be a checker gap worth investigating")
	}
}

func TestEmitRejectsOptionalIntegerToEnumReturnPosition(t *testing.T) {
	t.Parallel()
	// A return-position cast (`return 5 as ?Color;`) is rejected by the
	// CHECKER before the backend ever runs — a clean compile-time rejection,
	// not a backend crash and not a double-evaluated emission.
	if _, _, _, _, err := buildFixtureMaybeFailing(t, "type Color = enum { red, green, blue }; fn main() i32 {\nreturn 5 as ?Color;\n}", "main", false); err == nil {
		t.Errorf("checker accepted an integer-to-optional-enum cast in a return position")
	}
}

func TestEmitRejectsOptionalIntegerToEnumCallArgumentPosition(t *testing.T) {
	t.Parallel()
	// A cast used as a call argument (`helper(5 as ?Color)` where helper
	// takes a ?Color parameter) reaches the backend and is cleanly rejected
	// naming the parameter's optional type — the pre-existing optional-typed
	// parameter limitation, never a crash or a guessed lowering.
	unit, snapshot, entryID, _ := buildFixture(t, "type Color = enum { red, green, blue }; fn helper(c ?Color) i32 { return 0; } fn main() i32 {\nreturn helper(5 as ?Color);\n}", "main", false)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "parameter 0 (symbol")
}

func TestEmitRejectsNonScalarUnionPayload(t *testing.T) {
	t.Parallel()
	// A tagged-union payload that is not exactly the entry's resolved width,
	// bool, or str — a tuple, struct, array, optional, or nested enum — is
	// reachable from real source (the checker accepts such a variant
	// declaration and construction) but is a clean rejection naming what is
	// unsupported, never guessed at. The rejection happens in the union-type
	// collection walk, where each constructed variant's payload type is first
	// resolved from its construction site.
	emitAndRunRejects(t, "type C = union enum { empty void; value (i32, i32); }; fn main() i32 {\nvar c C = C.value((1, 2));\nreturn 0;\n}", "carries a payload of type (int, int); only a payload of i32, bool, or str is supported")
}

func TestEmitRejectsU64CheckedDivision(t *testing.T) {
	t.Parallel()
	// A u64 / or % has no checked runtime helper (div/mod is out of this
	// slice's scope — only +, -, * got u64 twins), so the backend must reject
	// it CLEANLY at Emit time, not emit a call to a nonexistent
	// pebble_rt_checked_div_u64/mod_u64 that would only fail at cc compile.
	// Both the plain expression form and the compound-assignment form are
	// asserted.
	emitAndRunRejects(t, "fn f() u64 { var a u64 = 10; var b u64 = 2; return a / b; } fn main() int { return f() as int; }", "only +, -, and * have a checked runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var a u64 = 10; var b u64 = 2; return a % b; } fn main() int { return f() as int; }", "only +, -, and * have a checked runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var x u64 = 10; x /= 2; return x; } fn main() int { return f() as int; }", "no checked division/modulo runtime helper")
	emitAndRunRejects(t, "fn f() u64 { var x u64 = 10; x %= 2; return x; } fn main() int { return f() as int; }", "no checked division/modulo runtime helper")
}

func TestEmitRejectsNarrowCheckedArithmetic(t *testing.T) {
	t.Parallel()
	// A checked +, -, *, /, or % at a narrow fixed-width integer (u8, u16,
	// i8, i16, or u32) has no runtime helper: checkedSuffix yields "" for
	// every width except int/i32/i64/u64, so the PLAIN BINARY EXPRESSION form
	// must be rejected CLEANLY at Emit time, not emitted as a call to a
	// nonexistent empty-suffix pebble_rt_checked_add_/sub_/etc. helper that
	// would only fail at cc compile. The error must name the operator and the
	// offending width. uint is deliberately NOT asserted here: uint-typed
	// CheckedArithmetic is lowered by buildUintExpr to plain C arithmetic and
	// never reaches this helper, so it is unaffected by the guard.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"add u8", "fn f() u8 { var a u8 = 200; var b u8 = 100; return a + b; } fn main() int { return f() as int; }", "operator + at u8"},
		{"sub u8", "fn f() u8 { var a u8 = 200; var b u8 = 100; return a - b; } fn main() int { return f() as int; }", "operator - at u8"},
		{"mul u8", "fn f() u8 { var a u8 = 200; var b u8 = 2; return a * b; } fn main() int { return f() as int; }", "operator * at u8"},
		{"div u8", "fn f() u8 { var a u8 = 200; var b u8 = 2; return a / b; } fn main() int { return f() as int; }", "operator / at u8"},
		{"mod u8", "fn f() u8 { var a u8 = 200; var b u8 = 2; return a % b; } fn main() int { return f() as int; }", "operator % at u8"},
		{"add u16", "fn f() u16 { var a u16 = 200; var b u16 = 100; return a + b; } fn main() int { return f() as int; }", "operator + at u16"},
		{"add i8", "fn f() i8 { var a i8 = 100; var b i8 = 20; return a + b; } fn main() int { return f() as int; }", "operator + at i8"},
		{"add i16", "fn f() i16 { var a i16 = 100; var b i16 = 20; return a + b; } fn main() int { return f() as int; }", "operator + at i16"},
		{"add u32", "fn f() u32 { var a u32 = 200; var b u32 = 100; return a + b; } fn main() int { return f() as int; }", "operator + at u32"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			emitAndRunRejects(t, tc.src, tc.want)
		})
	}
}

func TestEmitRejectsWrappingU64BuiltinNonU64Argument(t *testing.T) {
	t.Parallel()
	// A non-u64 argument to a wrapping builtin is a CLEAN rejection, never a
	// crash and never silently-wrong C: an i32-typed argument cannot satisfy
	// the builtin's u64 parameter, and Emit rejects the call with an error that
	// names the offending width ("want u64") instead of emitting a call that
	// would only fail at cc compile time. The symbol table is required for the
	// builtin identity lookup, so the fixture is built with symbols like the
	// positive case.
	unit, snapshot, entryID, sources, resolution := buildFixtureWithSymbols(t, `fn main() int { var x i32 = 6; var r u64 = wrapping_mul_u64(x, 7); return r as int; }`)
	var buf bytes.Buffer
	err := Emit(unit, snapshot, entryID, sources, resolution, &buf)
	if err == nil {
		t.Fatal("Emit succeeded for a non-u64 wrapping builtin argument, want rejection")
	}
	if buf.Len() != 0 {
		t.Fatalf("Emit wrote output on failure: %q", buf.String())
	}
	if !strings.Contains(err.Error(), "want u64") {
		t.Fatalf("Emit rejection error %q does not contain \"want u64\"", err.Error())
	}
}

func TestEmitRejectsMismatchedNonEntryWidthComparison(t *testing.T) {
	t.Parallel()
	// A comparison between two mismatched non-entry-width integers (u64 vs
	// u8) is a CLEAN rejection — an error, never a crash and never
	// silently-wrong C. The checker requires both comparison operands to carry
	// the identical concrete type (validateBooleanOperators rejects
	// typeIDs[0] != typeIDs[1]) and rejects the mismatch itself at
	// type-check time as a T0505 "cannot unify" — so from real source the
	// rejection happens before typed IR ever reaches Emit. The backend's own
	// same-width guard in buildComparison (mirroring the enum branch) is
	// defense-in-depth for hand-built IR that bypasses the checker.
	_, _, _, _, err := buildFixtureMaybeFailing(t, "fn main() int { let a u64 = 5; let b u8 = 3; let eq bool = a == b; if eq { return 0; } else { return 1; } }", "main", false)
	if err == nil {
		t.Fatal("expected the checker to reject a u64 == u8 comparison, but the fixture built and checked successfully")
	}
	if !strings.Contains(err.Error(), "check failed") {
		t.Fatalf("expected a clean check-phase rejection, got: %v", err)
	}
}

func TestEmitRejectsMethodCallSliceIndexOutOfBoundsAbnormalExit(t *testing.T) {
	t.Parallel()
	// Regression guard: bounds checking must not be silently dropped for the
	// new call-result base shape. An out-of-range index against a
	// freshly-computed slice base must still abort at runtime, exactly like
	// every other indexing path in this backend (see
	// TestEmitArrayOutOfBoundsAborts for the same wantAbnormal convention).
	// print, not return, is the supported position for this base shape (a
	// plain return of an indexed call-result slice is a clean, documented
	// rejection - the pre-threading this fix adds only covers positions
	// with a statement sequence to host the temp; print/buildLeadingStatement
	// is one, a scalar return via the general buildExpr path is not).
	emitAndRun(t, `type Bag = struct {
    data []i32;
    fn view(self Bag) []i32 { return self.data[:]; }
};
fn main() int {
    var a [3]i32 = [10, 20, 30];
    var b Bag = Bag.{ data = a[:] };
    print b.view()[10];
    return 0;
}`, false, 0, true)
}

func TestEmitSliceStructFieldInlineConstructionAsCallArgumentRejects(t *testing.T) {
	t.Parallel()
	// An inline slice construction in a pure expression position — a struct
	// value with such a field used as a call argument — is a clean rejection,
	// the same discipline buildSliceArgument applies to a bare CheckedSlice
	// call argument: a C function argument is a pure expression position with
	// nowhere to place the temp-declaration statement the construction needs.
	// The slice-typed-local-reference shape remains the supported spelling.
	emitAndRunRejects(t, `type Bag = struct { items []int; };
fn read(b Bag) int { return b.items[1]; }
fn main() int {
    var arr [3]int = [1, 2, 3];
    return read(Bag.{ items = arr[:] });
}`, "nowhere to place the temp-declaration statement")
}

func TestEmitSliceConstructionAsCallArgumentInReturnPositionCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// An inline slice construction used directly as a call argument (f(a[1:3]))
	// in a PURE EXPRESSION position — here a return value, `return f(a[1:3]);`
	// — is checker-reachable and, since the GNU statement-expression change,
	// supported: the construction's temp declaration and compound literal are
	// folded into a single statement-expression argument, `({ <temp decl>;
	// <compound literal>; })`, making the call a single primary expression valid
	// in the return's pure expression position (see buildSliceArgument /
	// sliceConstructionStatementExpr). Previously this exact shape was a clean
	// rejection pinning the expression-position boundary; the leading-statement
	// positions (a bare call statement or a local's declaration initializer)
	// were and remain supported through the pre-statement path (see
	// TestEmitSliceConstructionAsCallArgumentCompilesAndRuns). a[1:3] = [2, 3,
	// 4], so f returns x[0] = 2.
	emitAndRun(t, "fn f(x []i32) i32 { return x[0]; } fn main() i32 { var a [5]i32 = [1, 2, 3, 4, 5]; return f(a[1:3]); }", false, 2, false)
}

func TestEmitRejectsSliceParameterUnsupportedElementType(t *testing.T) {
	t.Parallel()
	// A slice-typed parameter whose element type is not the entry's width or
	// bool is a clean rejection from validateHelperSignature. A []str parameter
	// is checker-reachable but not constructible from real source (a str array
	// is itself rejected by the array element gate before any slice of it could
	// reach a call site), so this is hand-built through the IR builder to
	// exercise the gate directly: helper symbol 24 takes one []str parameter
	// (its type borrowed from a real checker-built fixture) and main calls it,
	// so the reachability walk hits the gate before any body is built.
	unit, snapshot, entryID := buildSliceOfStrParameterUnit(t)
	assertEmitRejectsContaining(t, unit, snapshot, entryID, "unsupported element type")
}
