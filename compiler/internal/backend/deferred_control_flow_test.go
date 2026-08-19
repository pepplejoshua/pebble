package backend

import "testing"

// Deferred control-flow tests (Phase 3 #30). Phase 3 #29 added tir.Block
// support to buildDeferredStatements, which already covers a conditional,
// loop, or switch INSIDE a deferred block (`defer { if ... }`) via its
// delegation to buildFallthroughBody. The distinct remaining gap this file
// covers: a BARE deferred control-flow statement with no enclosing block
// (`defer if ...`, `defer while ...`, `defer loop ...`, `defer for ...`,
// `defer switch ...`) — the parser accepts any statement after `defer`, and
// the checker's C0613 permits these kinds, but buildDeferredStatements had no
// case for tir.If/While/RangeLoop/For/Switch, so every bare form Emit-rejected
// even though the block-wrapped form already worked. Fixed by adding a case
// for each, delegating to the same builder (buildLoopIf/buildWhile/
// buildRangeLoop/buildFor/buildLoopSwitch) the non-deferred fall-through
// dispatch already uses, wrapped in a fresh C block mirroring V1's
// defer-local block exactly.

func TestDeferredBlockControlFlowCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// Regression guard: a conditional/loop/switch INSIDE a deferred block
	// already worked via Phase 3 #29's Block case — locked in here alongside
	// the new bare-form coverage below.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"if", "fn main() i32 { defer { if 1 == 1 { println 7; } else { println 8; } } return 0; }", "7\n"},
		{"while", "fn main() i32 { defer { var i i32 = 0; while i < 3 { println i; i = i + 1; } } return 0; }", "0\n1\n2\n"},
		{"loop", "fn main() i32 { defer { loop 0..3 : i { println i; } } return 0; }", "0\n1\n2\n"},
		{"switch", "fn main() i32 { defer { switch 2 { case 1: println 1; case 2: println 2; else: println 0; } } return 0; }", "2\n"},
		{"for", "fn main() i32 { defer { for var i i32 = 0; i < 3; i = i + 1 { println i; } } return 0; }", "0\n1\n2\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestDeferredBareControlFlowCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bare deferred control-flow statement — no enclosing `defer { }` block
	// — is the Phase 3 #30 fix. Each was Emit-rejected before it
	// ("... is not a supported deferred statement kind").
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"if", "fn main() i32 { defer if 1 == 1 { println 7; } else { println 8; } return 0; }", "7\n"},
		{"while", "fn main() i32 { var i i32 = 0; defer while i < 3 { println i; i = i + 1; } return 0; }", "0\n1\n2\n"},
		{"loop", "fn main() i32 { defer loop 0..3 : i { println i; } return 0; }", "0\n1\n2\n"},
		{"switch", "fn main() i32 { defer switch 2 { case 1: println 1; case 2: println 2; else: println 0; } return 0; }", "2\n"},
		{"for", "fn main() i32 { defer for var i i32 = 0; i < 3; i = i + 1 { println i; } return 0; }", "0\n1\n2\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestDeferredBareControlFlowBreakContinueCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A break/continue inside a bare deferred loop/switch must target the
	// construct contained within the deferred statement's own region
	// (regionHasEscapingExit), resolving to the same C break/continue.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"while-break", "fn main() i32 { var i i32 = 0; defer while i < 5 { if i == 2 { break; } println i; i = i + 1; } return 0; }", "0\n1\n"},
		{"while-continue", "fn main() i32 { var i i32 = 0; defer while i < 5 { i = i + 1; if i == 2 { continue; } println i; } return 0; }", "1\n3\n4\n5\n"},
		{"loop-break", "fn main() i32 { defer loop 0..5 : i { if i == 2 { break; } println i; } return 0; }", "0\n1\n"},
		{"switch-loop-break", "fn main() i32 { defer switch 1 { case 1: { var i i32 = 0; while i < 3 { if i == 1 { break; } println i; i = i + 1; } } else: println 9; } return 0; }", "0\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("output = %q, want %q", out, tc.want)
			}
		})
	}
}

func TestDeferredBareControlFlowNestedAndScopeCompilesAndRuns(t *testing.T) {
	t.Parallel()
	// A bare deferred control-flow statement can nest another control-flow
	// shape, can read an enclosing local, and still fires in LIFO order
	// alongside a plain deferred print.
	for _, tc := range []struct {
		name string
		src  string
		want string
	}{
		{"if-containing-while", "fn main() i32 { defer if 1 == 1 { var i i32 = 0; while i < 2 { println i; i = i + 1; } } return 0; }", "0\n1\n"},
		{"reads-enclosing-local", "fn main() i32 { var x i32 = 10; defer while x < 12 { println x; x = x + 1; } return 0; }", "10\n11\n"},
		{"lifo-with-plain-print", "fn main() i32 { defer while false { println 1; } defer println 2; return 0; }", "2\n"},
	} {
		t.Run(tc.name, func(t *testing.T) {
			t.Parallel()
			out := emitAndRunCapture(t, tc.src, false, 0, false)
			if out != tc.want {
				t.Fatalf("output = %q, want %q", out, tc.want)
			}
		})
	}
}
