package backend

import (
	"bytes"
	"strings"
	"testing"
)

// TestEmitDeferredBlockLocalDeclarationCompilesAndRuns proves a block-wrapped
// deferred local declaration (`defer { var x = 5; println x; }`) runs its
// declaration and the rest of the block at function exit: the deferred block's
// local x is declared and printed inside the defer-local scope, and nothing is
// emitted at the defer statement's own position. Before the backend gap fix
// this fixture Emit-rejected (the DeferRegister's Block child hit the
// unsupported-statement-kind default case).
func TestEmitDeferredBlockLocalDeclarationCompilesAndRuns(t *testing.T) {
	t.Parallel()
	out := emitAndRunCapture(t, "fn main() i32 { defer { var x i32 = 5; println x; } return 0; }", false, 0, false)
	if out != "5\n" {
		t.Fatalf("deferred block local output = %q, want %q", out, "5\n")
	}
}

// TestEmitDeferredBareLocalDeclarationLIFOCompilesAndRuns proves a bare
// deferred local declaration (`defer var x = init();`) runs its initializer at
// function exit in the same LIFO defer order the other deferred statement kinds
// use: the deferred println (registered second) fires first, then the deferred
// declaration's side-effecting initializer (registered first). Before the
// backend gap fix this fixture Emit-rejected (the DeferRegister's Initialize
// child hit the "not supported as a deferred statement yet" rejection).
func TestEmitDeferredBareLocalDeclarationLIFOCompilesAndRuns(t *testing.T) {
	t.Parallel()
	out := emitAndRunCapture(t, "fn initx() i32 { println 2; return 2; }\nfn main() i32 { defer var x i32 = initx(); defer println 1; return 0; }", false, 0, false)
	if out != "1\n2\n" {
		t.Fatalf("deferred LIFO output = %q, want %q", out, "1\n2\n")
	}
}

// TestEmitDeferredLocalDoesNotShadowEnclosingCompilesAndRuns proves a deferred
// local named x declared while an enclosing x is in scope is scoped to its own
// defer-local C block, exactly like V1's `{ /* defer */ ... }`: the deferred x
// never touches the enclosing x, so a later read of x sees the enclosing
// value, and the deferred declaration's initializer runs at exit without
// colliding with the enclosing declaration.
func TestEmitDeferredLocalDoesNotShadowEnclosingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	out := emitAndRunCapture(t, "fn main() i32 { var x i32 = 10; defer var x i32 = 99; println x; return 0; }", false, 0, false)
	if out != "10\n" {
		t.Fatalf("deferred local shadowing output = %q, want %q (enclosing x unchanged by deferred x)", out, "10\n")
	}
}

// TestEmitDeferredLocalInitializerReadsEnclosingCompilesAndRuns proves a
// deferred local's initializer can read an enclosing local: the defer-local
// scope layers on top of the enclosing scope, so y stays visible while the
// deferred x (declared from y + 4) is scoped to the defer.
func TestEmitDeferredLocalInitializerReadsEnclosingCompilesAndRuns(t *testing.T) {
	t.Parallel()
	out := emitAndRunCapture(t, "fn main() i32 { var y i32 = 3; defer { var x i32 = y + 4; println x; } return 0; }", false, 0, false)
	if out != "7\n" {
		t.Fatalf("deferred initializer read output = %q, want %q", out, "7\n")
	}
}

// TestEmitDeferredBlockFiresBeforeBreakCompilesAndRuns proves a deferred block
// local declaration registered inside a loop fires before the break's exit,
// through the same buildLoopJump DeferChain path a deferred Store uses: the
// deferred block's x is declared and printed just before the break.
func TestEmitDeferredBlockFiresBeforeBreakCompilesAndRuns(t *testing.T) {
	t.Parallel()
	out := emitAndRunCaptureBounded(t, "fn main() i32 { var i i32 = 0; while i < 5 { if i == 0 { defer { var x i32 = 7; println x; } break; } i = i + 1; } return 0; }", false, 0, false)
	if out != "7\n" {
		t.Fatalf("deferred block-on-break output = %q, want %q", out, "7\n")
	}
}

// TestEmitDeferredLocalDeclarationCOutput confirms the emitted C for a bare
// deferred local declaration: the declaration and its (void) cast appear
// inside a fresh C block (V1's defer-local block) immediately before the
// return, the declaration does not leak into the enclosing function scope, and
// the word 'defer' never reaches the emitted C.
func TestEmitDeferredLocalDeclarationCOutput(t *testing.T) {
	t.Parallel()
	unit, snapshot, entryID, sources := buildFixture(t, "fn main() i32 { defer var x i32 = 5; return 0; }", "main", false)
	var buf bytes.Buffer
	if err := Emit(unit, snapshot, entryID, sources, nil, &buf); err != nil {
		t.Fatalf("Emit failed: %v", err)
	}
	out := buf.String()
	// The declaration is block-scoped: an opening brace, the declaration, its
	// (void) cast, a closing brace, then the return — so the deferred local is
	// invisible outside the defer, and its declaration cannot collide with a
	// same-named local in the enclosing body.
	if !strings.Contains(out, "{\n        int32_t pebble_local_28 = 5;\n        (void)pebble_local_28;\n    }\n    return 0;") {
		t.Errorf("emitted C is missing the block-scoped deferred local declaration:\n%s", out)
	}
	if strings.Contains(out, "defer") {
		t.Errorf("emitted C contains the word 'defer':\n%s", out)
	}
	compileAndRunBounded(t, buf.Bytes(), 0, false)
}
