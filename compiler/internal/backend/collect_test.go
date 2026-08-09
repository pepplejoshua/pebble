package backend

import (
	"bytes"
	"strings"
	"testing"
)

func TestEmitGenericReachabilityUsesSpecializationIdentity(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn add_one[T](x T, y T) T => x; fn main() i32 { var a i32 = add_one[i32](40, 1); let p *i32 = &a; let b *i32 = add_one[*i32](p, p); return a + *b; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 80, false)
}

func TestEmitGenericReachabilityEmitsThreeSpecializations(t *testing.T) {
	unit, snapshot, entryID, sources := buildFixture(t, `fn choose[T](x T) i32 => 7; fn main() i32 { var a i32 = choose[i32](1); var b i32 = choose[bool](true); let p *i32 = &a; var c i32 = choose[*i32](p); return a + b + c; }`, "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	compileAndRun(t, buf.Bytes(), 21, false)
}

// TestEmitGenericHelperSpecializedAtConcreteWidthCompilesAndRuns is the exact
// motivating repro for the compatible-integer-width parameter gate: a generic
// helper identity[T] called with an i32 local from an fn main() int entry
// (whose own resolved width is the abstract int builtin, NOT i32 — the two are
// distinct builtins sharing the int32_t C representation). The specialization
// identity[i32] has an i32-typed parameter, which the pre-fix
// validateHelperSignature rejected with "called function symbol ... has type
// i32, want int". The program must compile under -Wall -Wextra -Werror and run,
// returning the value identity passed through.
func TestEmitGenericHelperSpecializedAtConcreteWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn identity[T](x T) T { return x; } fn main() int { var a i32 = 5; var r = identity(a); return r; }`, false, 5, false)
}

// TestEmitGenericClampShapeSpecializedAtConcreteWidthCompilesAndRuns mirrors
// the real std/math.peb motivating case (clamp[T] = max(lo, min(x, hi))) with
// the generic helper DEFINED INLINE so the test needs no std import: a
// two-level generic chain (min/max) whose specializations all substitute the
// concrete i32 width, called from an fn main() int entry with i32 locals and
// the result stored into an i32 local before returning (the shape that reaches
// emission — a direct `return clamp(...)` from an int entry hits a checker-level
// int-vs-i32 unification conflict instead). The clamp of (5, 10, 20) is 10.
func TestEmitGenericClampShapeSpecializedAtConcreteWidthCompilesAndRuns(t *testing.T) {
	emitAndRun(t, `fn min[T](a T, b T) T { if a < b { return a; } return b; } fn max[T](a T, b T) T { if a > b { return a; } return b; } fn clamp[T](x T, lo T, hi T) T { return max(lo, min(x, hi)); } fn main() int { var x i32 = 5; var lo i32 = 10; var hi i32 = 20; var r i32 = clamp(x, lo, hi); return r; }`, false, 10, false)
}

func TestEmitRecursionWritesPrototypesBeforeDefinitions(t *testing.T) {
	// The emitted-C shape for the three-hop cycle: every reachable helper must
	// be forward-declared (a static prototype ending in `;`) BEFORE any
	// helper definition (a static function ending in `{`), and each definition
	// must come after its own prototype — that is the mechanism that makes the
	// recursive calls legal C regardless of definition order. In particular,
	// rehash calls insert, whose definition follows rehash's, so insert's
	// prototype must appear before rehash's definition for the emitted C to
	// compile warning-free under -Wall -Wextra -Werror.
	unit, snapshot, entryID, sources := buildFixture(t, "fn insert(n i32) i32 { if n == 0 { return 0; } else { return maybe_grow(n); } } fn maybe_grow(n i32) i32 { if n > 10 { return n; } else { return rehash(n); } } fn rehash(n i32) i32 { return insert(n + 1); } fn main() i32 { return insert(1); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// Every helper has parameters, so the prototype and the definition both
	// begin "static int32_t pebble_fn_<id>(PebbleContext *ctx" and differ only
	// in their terminator (the prototype ends its parameter list with a `;`,
	// the definition with a `{`). Walk the shared prefix from each occurrence
	// to the next `;` or `{` to classify it.
	var firstDefinitionAt, lastPrototypeAt int
	for _, symbolID := range []string{"24", "26", "28"} {
		prefix := "static int32_t pebble_fn_" + symbolID + "(PebbleContext *ctx"
		var prototypeAt, definitionAt int
		for from := 0; ; {
			index := strings.Index(out[from:], prefix)
			if index < 0 {
				break
			}
			absolute := from + index
			rest := out[absolute+len(prefix):]
			if semi := strings.Index(rest, ";"); semi >= 0 && (strings.Index(rest, "{") < 0 || semi < strings.Index(rest, "{")) {
				prototypeAt = absolute
			} else if brace := strings.Index(rest, "{"); brace >= 0 {
				definitionAt = absolute
			}
			from = absolute + len(prefix)
		}
		if prototypeAt == 0 {
			t.Errorf("emitted C missing prototype for pebble_fn_%s:\n%s", symbolID, out)
		}
		if definitionAt == 0 {
			t.Errorf("emitted C missing definition for pebble_fn_%s:\n%s", symbolID, out)
		}
		if prototypeAt >= definitionAt {
			t.Errorf("prototype for pebble_fn_%s does not precede its definition:\n%s", symbolID, out)
		}
		if firstDefinitionAt == 0 || definitionAt < firstDefinitionAt {
			firstDefinitionAt = definitionAt
		}
		if prototypeAt > lastPrototypeAt {
			lastPrototypeAt = prototypeAt
		}
	}
	// Every prototype must come before every definition: the last prototype's
	// `;` must precede the first definition's `{`.
	if firstDefinitionAt == 0 || lastPrototypeAt == 0 {
		t.Fatalf("emitted C has no helper prototypes/definitions:\n%s", out)
	}
	if lastPrototypeAt >= firstDefinitionAt {
		t.Errorf("a prototype does not precede all definitions:\n%s", out)
	}
}

func TestEmitUnreachableFunctionNotEmitted(t *testing.T) {
	// A declared function the entry never calls, directly or transitively, must
	// not be emitted at all — the generated C has no trace of it (symbol 25,
	// the unused function), so the -Wall -Wextra -Werror build cannot warn
	// about an unused static function. Only the reachable helper (symbol 24)
	// is emitted, and the program runs to exit 21.
	unit, snapshot, entryID, sources := buildFixture(t, "fn helper() i32 { return 21; } fn unused() i32 { return 99; } fn main() i32 { return helper(); }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	if !strings.Contains(out, "pebble_fn_24") {
		t.Errorf("emitted C missing the reachable helper:\n%s", out)
	}
	if strings.Contains(out, "pebble_fn_25") {
		t.Errorf("emitted C contains the unreachable function (symbol 25), which would trigger -Wunused-function:\n%s", out)
	}
	binary := compileEmittedC(t, buf.Bytes())
	runCompiledBinary(t, binary, 21, false, false)
}
