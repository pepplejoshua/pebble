package backend

import (
	"fmt"
	"strconv"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/source"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// buildBlock validates one block under the entry body's recursive grammar and
// builds its C statement sequence. A block is zero or more `<cType> <width>
// pebble_local_<id>` declarations (one per Initialize, in declaration order),
// zero or more `pebble_local_<id> = <built value>;` reassignments (one per
// Store, targeting a local already in scope), and zero or more `while (...)
// { <loop body> }` loop statements (one per While, built by buildWhile), zero
// or more `for (<init>; <cond>; <update>) { <loop body> }` classic for loop
// statements (one per For, built by buildFor), and zero or more range loop
// statements (one per RangeLoop, built by buildRangeLoop) — a
// loop is a leading statement here except for one terminal shape — a while
// is only ever a leading statement unless it is the block's final statement
// and exhaustive (see the tail switch's While case) —
// followed by a tail that is either the single `return <expression>;`, a
// two-armed if/else built by buildIf, a switch statement built by
// buildSwitch, or a terminal exhaustive `while true` loop built by the same
// buildWhile; each if arm and each case body is itself a block under the
// same grammar, so buildBlock recurses into both arms and case bodies.
// width is the entry's
// resolved integer width (types.Int, types.I32, or types.I64), threaded through to every
// expression and declaration built here so the emitted C type names and
// runtime helper names follow the width. locals is the set of
// symbols visible at the block's entry (the enclosing scopes' declarations)
// and is copied at entry: every addition this block makes — its own
// declarations, and anything an arm's or a loop body's subtree declares —
// stays in that copy and never mutates the map the caller or a sibling scope
// sees. That copy-per-scope discipline is what makes a local declared inside
// one arm (or inside a loop body) invisible to its siblings and to any scope
// outside it, while locals declared in an enclosing block remain visible
// inside. depth is the nesting level of this block below the function body (0
// for the entry body itself); statements and the if/else braces are indented
// one level per depth so nested output stays well-formed C. Any other shape is
// rejected with a descriptive error, not best-effort lowered.
func buildBlock(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, blockID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	block, ok := unit.Node(blockID)
	if !ok {
		return "", fmt.Errorf("entry function body references invalid block node %d", blockID)
	}
	if block.Kind != tir.Block {
		return "", fmt.Errorf("entry function body block is a %s, want a Block", block.Kind)
	}
	if len(block.Children) == 0 {
		return "", fmt.Errorf("entry function body block is empty, want zero or more local declarations or reassignments followed by exactly one return or a two-armed if/else")
	}
	scope := cloneLocals(locals)
	indent := strings.Repeat("    ", depth+1)
	var statements []string
	for i := 0; i < len(block.Children)-1; i++ {
		statement, ok := unit.Node(block.Children[i])
		if !ok {
			return "", fmt.Errorf("entry function body block references invalid statement node %d", block.Children[i])
		}
		if statement.Kind == tir.While {
			// A while loop is a leading statement in the block grammar, the
			// tail only when it is the block's final statement and exhaustive
			// (see the tail switch's While case): a leading while runs its
			// body (which may itself declare locals and
			// reassign enclosing ones) as many times as its condition holds,
			// then control falls through to the statements after it. The loop
			// body is its own scope (buildWhile clones, exactly as buildIf's
			// arms do), so nothing the loop declares leaks into this block's
			// scope map.
			whileText, err := buildWhile(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, whileText)
			continue
		}
		if statement.Kind == tir.RangeLoop {
			// A range loop is a leading statement in the block grammar exactly
			// like a while — never the tail — lowering to a C for loop over the
			// bound iterator. Its body is its own scope (buildRangeLoop seeds
			// the iterator and buildLoopBody clones), so nothing the loop
			// declares leaks into this block's scope map.
			rangeText, err := buildRangeLoop(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, rangeText)
			continue
		}
		if statement.Kind == tir.For {
			// A classic for loop is a leading statement in the block grammar
			// exactly like a while — never the tail — lowering to a C for loop
			// with the same three individually-optional clauses. Its body is
			// its own scope (buildFor seeds the initializer's local and
			// buildLoopBody clones), so nothing the loop declares leaks into
			// this block's scope map.
			forText, err := buildFor(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, forText)
			continue
		}
		if statement.Kind == tir.DeferRegister {
			// A DeferRegister in a block's leading-statement sequence is a
			// registration marker the checker's analysis already consumed; the
			// backend must emit nothing at this position. The deferred statement
			// is only ever emitted at exit points whose DeferChain references it.
			continue
		}
		text, err := buildLeadingStatement(unit, snapshot, fileSet, block.Children[i], scope, indent, depth, "entry function body block", width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, text)
	}
	last, ok := unit.Node(block.Children[len(block.Children)-1])
	if !ok {
		return "", fmt.Errorf("entry function body block references invalid statement node %d", block.Children[len(block.Children)-1])
	}
	switch last.Kind {
	case tir.Return:
		// A return in the block's tail position — the enclosing function's
		// final statement. The return value's grammar, the deferred statements
		// that must run first, and the slice-construction temp-then-construction
		// shape are all built by the shared buildReturnStatement (also used by
		// buildFallthroughBody for a return inside a fall-through statement
		// sequence), so the emission logic lives in exactly one place.
		text, err := buildReturnStatement(unit, snapshot, fileSet, last, scope, indent, "entry function body block", width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, text)
	case tir.ImplicitReturn:
		// A void-result function (a reachable void helper — the entry's own
		// void shape is handled separately by validateEmptyBody and never
		// reaches buildBlock) ends its body with a synthesized ImplicitReturn,
		// confirmed against real fixtures. It emits nothing: the C function
		// simply falls off the end, which is legal for a `static void`
		// function. Any DeferChain on it is emitted first (a defer registered
		// inside a void helper must still fire at the helper's exit). A
		// non-void function can never end in an ImplicitReturn from real
		// source (the checker rejects fall-through with C0607), so this case
		// only ever fires for result.kind == types.Void; a hand-built non-void
		// block ending in ImplicitReturn is a clean rejection.
		if result.kind != types.Void {
			return "", fmt.Errorf("entry function body block statement is an ImplicitReturn, want a Return of an integer expression, a two-armed if/else, or a switch (an implicit fall-through tail only appears in a void function, but the enclosing function resolves to a non-void result)")
		}
		deferText, err := buildDeferredStatements(unit, snapshot, fileSet, last.DeferChain, scope, indent, "entry function body block", width, unions)
		if err != nil {
			return "", err
		}
		if deferText != "" {
			statements = append(statements, deferText)
		}
	case tir.If:
		ifText, err := buildIf(unit, snapshot, fileSet, last, scope, depth, width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, ifText)
	case tir.Switch:
		switchText, err := buildSwitch(unit, snapshot, fileSet, last, scope, depth, width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, switchText)
	case tir.While:
		// A terminal while — the final statement of a non-void body whose IR
		// ends in a raw While (the IR builder omits the ImplicitReturn tail
		// for non-void callables; a void body always ends in the ImplicitReturn
		// the checker appends, so this case only fires for non-void bodies or
		// hand-built IR) — is accepted only when the loop can never fall
		// through: its condition is the literal `true` and its loop-body
		// subtree contains no Break targeting this loop's own region (the
		// checker's constant-true-loop acceptance predicate, in shape form, per
		// terminalWhileIsExhaustive). Such a loop is lowered by the exact same
		// buildWhile a leading while uses, with no synthetic return: every exit
		// from the loop is a return, so the C function never falls off the end
		// and a missing return line is not a defect. Any other terminal while —
		// a comparison condition, a constant-true loop with a break to this
		// loop, or a genuinely fall-through body — cannot satisfy a non-void
		// result and stays rejected with the same message the default case
		// below would produce.
		if terminalWhileIsExhaustive(unit, last) {
			whileText, err := buildWhile(unit, snapshot, fileSet, last, scope, depth, width, result, unions)
			if err != nil {
				return "", err
			}
			statements = append(statements, whileText)
		} else {
			return "", fmt.Errorf("entry function body block statement is a %s, want a Return of an integer expression, a two-armed if/else, or a switch", last.Kind)
		}
	default:
		return "", fmt.Errorf("entry function body block statement is a %s, want a Return of an integer expression, a two-armed if/else, or a switch", last.Kind)
	}
	return strings.Join(statements, "\n"), nil
}

// terminalWhileIsExhaustive reports whether a tir.While can serve as the final
// statement of a non-void body block without a trailing return: whether it can
// never fall through. It is the backend's conservative, shape-based form of the
// checker's constant-true-loop acceptance predicate (the checker accepts a
// while whose condition is a known constant true and that contains no break;
// see control_flow_validation.go's infinite/breakFound analysis). A terminal
// while is exhaustive exactly when two conditions both hold:
//
//   - the loop's condition is the literal boolean true — a tir.BoolLiteral with
//     Literal.Bool set, the exact condition real `while true { ... }` source
//     produces. A comparison condition (`while i < 5 { ... }`) can exit
//     normally and is not exhaustive; a constant-folded-but-not-literal shape
//     (`while (1 == 1) { ... }`) is intentionally narrower and stays rejected,
//     so this predicate does not need the checker's constant folding.
//
//   - the loop-body subtree contains no tir.Break whose Target is the loop's
//     own Region (the While node's Region). A break targeting this loop is the
//     one remaining way control can leave the loop and fall through past it,
//     so its presence means the loop is not exhaustive even though the
//     condition is constant true. A break targeting an enclosing loop or a
//     switch's region is fine — it does not exit this loop.
//
// The subtree walk follows Children and DeferChain, the same reachability
// pattern collectDirectCalls uses, skipping a DeferRegister at its registration
// position so the deferred statement is only examined once (via the exit
// point's DeferChain). Any malformed shape — a missing child, a wrong child
// count, an invalid node reference — reports false (not exhaustive), so the
// caller falls back to its normal clean rejection rather than a panic or a
// partial lowering.
func terminalWhileIsExhaustive(unit *tir.Unit, whileNode tir.Node) bool {
	if len(whileNode.Children) != 2 {
		return false
	}
	condition, ok := unit.Node(whileNode.Children[0])
	if !ok {
		return false
	}
	if condition.Kind != tir.BoolLiteral || !condition.Literal.Bool {
		return false
	}
	hasBreak, valid := loopBodyHasBreakTargeting(unit, whileNode.Children[1], whileNode.Region)
	if !valid {
		return false
	}
	return !hasBreak
}

// loopBodyHasBreakTargeting walks one node's subtree — Children and DeferChain,
// the same reachability pattern collectDirectCalls uses — looking for a
// tir.Break whose Target is loopRegion. It reports whether such a break was
// found and whether the subtree was fully traversable (false on any invalid
// node reference, so the caller treats an incomplete walk as not exhaustive).
func loopBodyHasBreakTargeting(unit *tir.Unit, nodeID tir.NodeID, loopRegion tir.RegionID) (found, valid bool) {
	node, ok := unit.Node(nodeID)
	if !ok {
		return false, false
	}
	if node.Kind == tir.Break && node.Target == loopRegion {
		return true, true
	}
	for _, childID := range node.Children {
		if child, ok := unit.Node(childID); ok && child.Kind == tir.DeferRegister {
			// A DeferRegister child is a registration marker whose statement
			// is reached via exit-point DeferChains, exactly as collectDirectCalls
			// skips it here, so the deferred statement is examined once.
			continue
		}
		childFound, childValid := loopBodyHasBreakTargeting(unit, childID, loopRegion)
		if !childValid {
			return false, false
		}
		if childFound {
			return true, true
		}
	}
	for _, deferID := range node.DeferChain {
		deferFound, deferValid := loopBodyHasBreakTargeting(unit, deferID, loopRegion)
		if !deferValid {
			return false, false
		}
		if deferFound {
			return true, true
		}
	}
	return false, true
}

// buildReturnStatement validates and builds the C text for one return
// statement (tir.Return) in any position: the tail of a block (buildBlock),
// or a return inside a fall-through statement sequence — an if arm, a switch
// case body, or a loop body (see buildFallthroughBody). The return value is
// built under the grammar the enclosing function's result selects:
// buildCharOperand for a char result, buildStrOperand for a str result,
// buildAggregateReturnValue for a tuple/struct result, buildSliceReturnValue
// for a slice result (whose fresh-construction shape needs the two-statement
// temp-then-construction form, threaded in as a pre-return statement),
// buildFloatExpr for a float result, and buildExpr for a scalar integer
// result. The DeferChain's deferred statements are emitted first (in the
// chain's own LIFO order), then the return line itself:
//
//	<indent>return <value>;
//
// with any pre-return statements and deferred statements joined ahead of it.
// Any other shape — a return with a child count other than exactly one — is a
// clean rejection naming what was found. context names the enclosing
// construct in error messages.
func buildReturnStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, returnNode tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(returnNode.Children) != 1 {
		if len(returnNode.Children) == 0 && result.kind == types.Void {
			// A bare `return;` inside a void-returning helper's body — the
			// std/hmap.peb maybe_grow shape (`if self.cap == 0 { self.rehash(8);
			// return; }`): a return with no value is only legal in a void
			// function, and lowers to a plain C `return;` after any deferred
			// statements fire, exactly as the void helper's ImplicitReturn tail
			// emits nothing but its deferred statements.
			deferText, err := buildDeferredStatements(unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, unions)
			if err != nil {
				return "", err
			}
			if deferText != "" {
				return deferText + "\n" + indent + "return;", nil
			}
			return indent + "return;", nil
		}
		return "", fmt.Errorf("%s return statement has %d argument(s), want exactly one expression", context, len(returnNode.Children))
	}
	var returnValue string
	var err error
	var preReturn string
	if result.kind == types.Bool {
		// The enclosing function returns bool (a reachable helper whose
		// ResultType is bool, added for the function-types slice), so the
		// return value is built under the bool grammar by buildBoolExpr rather
		// than buildExpr, which rejects a bool-typed value. Supported return
		// shapes are a SymbolValue naming a bool-typed local in scope, a bool
		// literal, a comparison, a ! negation, or an && / || combination.
		returnValue, err = buildBoolExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isChar {
		// The enclosing function returns char (a reachable helper whose
		// ResultType is char — the entry always threads a scalar resultInfo),
		// so the return value is built under the char grammar by
		// buildCharOperand rather than buildExpr, which rejects a
		// char-typed value. Supported return shapes are a SymbolValue
		// naming a char-typed local in scope, a char literal, or a call to
		// another char-returning helper.
		returnValue, err = buildCharOperand(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isStr {
		// The enclosing function returns str (a reachable helper whose
		// ResultType is str — the entry always threads a scalar resultInfo),
		// so the return value is built under the str grammar by
		// buildStrOperand rather than buildExpr, which rejects a str-typed
		// value. Supported return shapes are a SymbolValue naming a
		// str-typed local in scope, a string literal, or a call to another
		// str-returning helper.
		returnValue, err = buildStrOperand(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.tuple != 0 || result.structType != 0 {
		// The enclosing function returns a tuple/struct (a reachable helper
		// whose ResultType is an aggregate — the entry always threads a
		// scalar resultInfo), so the return value is built under the
		// aggregate grammar by buildAggregateReturnValue rather than
		// buildExpr, which rejects an aggregate-typed value. Supported
		// return shapes are a SymbolValue naming an aggregate-typed local
		// in scope of the matching type, a fresh inline TupleValue /
		// RecordConstruct of the matching type (both built via 10.25's
		// expression builders), or a DirectCall to a struct-returning helper
		// (a return forward); anything else is a clean rejection. The
		// builder returns a (pre, expr) pair — the DirectCall shape's
		// construction pre is threaded into the statement sequence ahead of
		// the final return line, exactly as the slice path below does.
		preReturn, returnValue, err = buildAggregateReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
	} else if result.arrayType != 0 {
		returnValue, err = buildArrayReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result.arrayType, width)
	} else if result.sliceType != 0 {
		// The enclosing function returns a slice (a reachable helper whose
		// ResultType is a slice type), so the return value is built under
		// the slice grammar by buildSliceReturnValue rather than buildExpr,
		// which rejects a slice-typed value. Supported return shapes are a
		// SymbolValue naming a slice-typed local in scope (a single-
		// statement forward) or a fresh CheckedSlice construction, which
		// needs the same two-statement temp-then-construction shape a slice
		// local's declaration uses. The temp-declaration statement is
		// threaded into the statement sequence as an extra pre-return
		// statement, the same mechanical shape the deferred statements
		// below demonstrate — just for construction complexity rather than
		// deferred cleanup.
		preReturn, returnValue, err = buildSliceReturnValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
	} else if result.optionalType != 0 {
		// The enclosing function returns an optional (a reachable helper
		// whose ResultType is an optional type), so the return value is
		// built under the optional grammar by buildOptionalValue
		// rather than buildExpr, which rejects an optional-typed value.
		// Supported return shapes are a SymbolValue naming an
		// optional-typed local in scope of the matching type, a fresh
		// SomeOptional / NoneOptional / OptionalInject construction of the
		// matching type (built by the shared buildOptionalValueExpr), a
		// DirectCall to another optional-returning helper (a return
		// forward), or a bare payload value whose implicit injection into
		// the optional is supplied here; anything else is a clean
		// rejection.
		returnValue, err = buildOptionalValue(unit, snapshot, fileSet, returnNode.Children[0], scope, result.optionalType, "entry function body return statement", width)
	} else if result.functionType != 0 {
		// The enclosing function returns a function type (a reachable helper
		// whose ResultType is a function type — the entry always threads a
		// scalar resultInfo), so the return value is built under the function
		// grammar by buildFunctionValue rather than buildExpr, which rejects
		// a function-typed value. Supported return shapes are a bare function
		// reference (a HoistedFunctionValue), a reference to an in-scope
		// function-typed local or parameter (a SymbolValue), a function-typed
		// struct field read (a FieldValue or Load(FieldPlace)), a generic
		// function referenced as a value (a GenericFunctionValue), or a call
		// to another function-returning helper (a DirectCall whose result type
		// is the function type — a return forward); anything else is a clean
		// rejection.
		returnValue, err = buildFunctionValue(unit, snapshot, fileSet, mustNode(unit, returnNode.Children[0]), scope, "entry function body return statement", width)
	} else if result.kind == types.F32 || result.kind == types.F64 {
		// A float-returning entry (a main declared to return f32/f64 — the
		// one float-returning position Float Stage A supports; float helper
		// results are rejected upstream by validateHelperSignature, so only
		// the entry's resultInfo can carry a float kind), so the return
		// value is built under the float grammar by buildFloatExpr rather
		// than buildExpr, which rejects a float-typed value. Supported
		// return shapes are a float literal or a SymbolValue naming a
		// float-typed local in scope of the same float kind.
		returnValue, err = buildFloatExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, result.kind)
	} else if result.kind == types.Uint {
		// A uint-returning helper (a reachable helper whose ResultType is
		// uint — helperSignature records resultInfo{kind: types.Uint} and
		// declares the C return type uint64_t), so the return value is built
		// under the uint grammar by buildUintExpr rather than buildExpr,
		// which rejects a uint-typed checked-arithmetic tree over a
		// SizeofType operand and has no uint arithmetic. Supported return
		// shapes are a SymbolValue naming a uint-typed local in scope, a
		// uint-typed checked-arithmetic tree, a sizeof result, or an
		// integer literal.
		returnValue, err = buildUintExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else {
		returnValue, err = buildExpr(unit, snapshot, fileSet, returnNode.Children[0], scope, width, width)
	}
	if err != nil {
		return "", err
	}
	deferText, err := buildDeferredStatements(unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, unions)
	if err != nil {
		return "", err
	}
	parts := []string{}
	if preReturn != "" {
		parts = append(parts, preReturn)
	}
	if deferText != "" {
		parts = append(parts, deferText)
	}
	parts = append(parts, indent+"return "+returnValue+";")
	return strings.Join(parts, "\n"), nil
}

// buildSwitch validates and builds the C text for a switch statement used as a
// block's tail: a tir.Switch whose Children[0] is the subject expression
// (built by buildExpr for an integer subject, buildBoolExpr for a bool
// subject, buildEnumValue for a plain enum subject — the enum subject
// grammar added by 10.34 — or, since 10.35, the .tag field read of a
// tagged-union subject, whose value is a local reference or an inline variant
// construction built as the union's compound literal) and Children[1:] are
// SwitchCase nodes. Each case
// value becomes its
// own C case label; multiple SwitchCase nodes sharing the same body node ID (a
// multi-value `case v1, v2:` clause) become stacked C case labels sharing one
// body. An else arm (HasElse) maps to C's `default:`. The emitted text is:
//
//	<indent>switch (<subject>) {
//	<indent>    case <v1>:
//	<indent>    case <v2>: {
//	<indent>        <body>
//	<indent>    }
//	<indent>    default: {
//	<indent>        <body>
//	<indent>    }
//	<indent>}
//
// A SwitchCase's body child may be a Block (a multi-statement case body
// requiring braces in the source) or a bare statement (a single-statement case
// body with no braces). A Block body is built via buildBlock at the next
// nesting depth; a bare statement is built directly. Every case body must end
// in a return or a two-armed if/else whose arms each end in return — the same
// tail-statement grammar buildBlock enforces for every other block in this
// backend. A CaseValue-based case (an enum variant) is supported since 10.34
// when the subject is a plain enum type or, since 10.35, a tagged-union type:
// such a case's label is
// `case pebble_variant_<caseValue>:` (see buildCaseLabel), the case value's C
// enum constant, and its value (the variant's ordinal in the enum's declared
// order) matches the subject's own typedef by construction. Any other shape is
// a clean rejection naming what was found.
//
// buildSwitch is the thin tail-position entry point: it builds every case
// body under buildSwitchCaseBody's "must end in return" grammar. The
// fall-through twin, buildLoopSwitch, reuses this same core via
// buildSwitchStatement with fallthrough set, so the subject-building and
// case-grouping logic lives in exactly one place.
func buildSwitch(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, false)
}

// buildLoopSwitch validates and builds the C text for a switch statement used
// as a non-tail, fall-through statement — a leading statement in a top-level
// function body, a statement inside a loop body, an if arm, or another switch
// case body. It is the fall-through twin of buildSwitch: everything about
// subject-building and case grouping is shared through buildSwitchStatement,
// and only the per-case body requirement differs — a case body is an ordinary
// statement sequence that MAY end in a return but may also simply fall
// through (see buildLoopSwitchCaseBody), instead of buildSwitchCaseBody's
// every-arm-must-return grammar. It serves both the top-level leading
// position (buildLeadingStatement's Switch case) and the loop-body/arm
// position (buildFallthroughStatement's Switch case): the two have no
// substantive difference, since a break/continue inside a case body targets
// the nearest enclosing loop or switch by Pebble's own control-flow rules,
// and the emitted C break/continue resolves to the same construct.
func buildLoopSwitch(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, true)
}

// buildSwitchStatement is the shared core behind buildSwitch and
// buildLoopSwitch: it validates and builds the C text for a switch statement
// with exactly the same subject-building, case-grouping, and label emission in
// both positions. The only difference is the case-body builder selected by
// fallthrough: false selects buildSwitchCaseBody (each body must end in a
// return), true selects buildLoopSwitchCaseBody (each body is an ordinary
// fall-through statement sequence that may or may not return).
func buildSwitchStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if len(switchNode.Children) < 2 {
		return "", fmt.Errorf("switch statement has %d child(ren), want at least 2 (the subject and one case)", len(switchNode.Children))
	}
	// Build the subject expression. The subject's resolved type decides the
	// grammar: an integer subject (the entry's width) is built by buildExpr,
	// a bool subject by buildBoolExpr, a char subject by buildCharOperand, a
	// tagged-union subject by
	// buildUnionConstruction (reading its .tag field), a plain enum subject
	// by buildEnumValue.
	// The subject type is on the subject node itself (Children[0]).
	subjectNode, ok := unit.Node(switchNode.Children[0])
	if !ok {
		return "", fmt.Errorf("switch statement references invalid subject node %d", switchNode.Children[0])
	}
	// enumSubject is nonzero exactly when the subject is an enum-typed type
	// (a plain enum or a tagged union); it governs the case labels (CaseValue
	// cases become `case pebble_variant_<caseValue>:`).
	var enumSubject types.TypeID
	var enumVariants []symbol.SymbolID
	if isEnumType(unit, snapshot, subjectNode.Type) {
		enumSubject = subjectNode.Type
		info, err := resolveEnumInfo(unit, snapshot, enumSubject)
		if err != nil {
			return "", err
		}
		enumVariants = info.variants
	}
	var subjectExpr string
	var err error
	if enumSubject != 0 {
		if _, isUnion := unions[enumSubject]; isUnion {
			// A tagged-union-typed subject: its value is the union's tagged
			// struct, so the switch compares the stored discriminant — the
			// tag field — against the case labels. A reference to a
			// union-typed local (a SymbolValue) is read as
			// `pebble_local_<sym>.tag`; a variant construction used directly as
			// the subject (switch Choice.value(5), confirmed checker-reachable)
			// is built as the union's compound literal and its .tag field read
			// the same way. The case labels are unchanged: `case
			// pebble_variant_<caseValue>:` names the same enum constant the
			// stored tag holds (see buildCaseLabel).
			switch subjectNode.Kind {
			case tir.SymbolValue:
				info, declared := locals[subjectNode.Symbol]
				if !declared || info.enumType == 0 {
					return "", fmt.Errorf("switch subject references symbol %d, which is not an enum-typed local declared earlier in the body", subjectNode.Symbol)
				}
				if info.enumType != enumSubject {
					return "", fmt.Errorf("switch subject references symbol %d, a local of type %s, not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, info.enumType), unionTypeName(enumSubject))
				}
				subjectExpr = fmt.Sprintf("pebble_local_%d.tag", subjectNode.Symbol)
			case tir.VariantConstruct, tir.EnumVariantValue:
				construction, buildErr := buildUnionConstruction(unit, snapshot, fileSet, subjectNode, locals, "switch subject", unions[enumSubject], width)
				if buildErr != nil {
					return "", buildErr
				}
				subjectExpr = construction + ".tag"
			default:
				return "", fmt.Errorf("switch subject is a %s of tagged-union type %s, want a reference to a union-typed local in scope or a union variant construction", subjectNode.Kind, unionTypeName(enumSubject))
			}
		} else {
			// A plain-enum-typed subject: a reference to an enum-typed local
			// (a SymbolValue) or a variant literal (an EnumVariantValue /
			// zero-payload VariantConstruct) — buildEnumValue handles all three.
			subjectExpr, err = buildEnumValue(unit, snapshot, fileSet, switchNode.Children[0], locals, width)
		}
	} else if isWidth(snapshot, width, subjectNode.Type) {
		subjectExpr, err = buildExpr(unit, snapshot, fileSet, switchNode.Children[0], locals, width, width)
	} else if isBool(snapshot, subjectNode.Type) {
		subjectExpr, err = buildBoolExpr(unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	} else if isChar(snapshot, subjectNode.Type) {
		// A char-typed subject: built by buildCharOperand, the same builder
		// every other char-typed position in this backend uses. A char's C
		// type is the fixed int32_t, so the subject is an integral value the
		// C switch can compare against char-literal case labels (emitted by
		// buildCaseLabel as `case (int32_t)<scalar>:`).
		subjectExpr, err = buildCharOperand(unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	} else if subjectNode.Kind == tir.IntegerLiteral && subjectNode.Type == snapshot.Builtins().Int {
		// An int-typed integer literal as the subject: the checker leaves
		// it as the unanchored int builtin when no width-anchoring position
		// is available. Lowered directly as its decimal text, the same
		// precedent buildComparisonOperand and buildRangeBound use.
		text := subjectNode.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("switch subject contains an integer literal with malformed text %q", text)
		}
		subjectExpr = text
	} else if subjectNode.Kind == tir.SymbolValue && subjectNode.Type == snapshot.Builtins().Int {
		// An int-typed SymbolValue: only reachable from hand-built IR in
		// this backend's grammar (no real source produces an int-typed
		// local), but accepted for completeness.
		if _, declared := locals[subjectNode.Symbol]; !declared {
			return "", fmt.Errorf("switch subject references symbol %d, which is not a local in scope", subjectNode.Symbol)
		}
		subjectExpr = fmt.Sprintf("pebble_local_%d", subjectNode.Symbol)
	} else {
		return "", fmt.Errorf("switch subject has type %s, want %s, bool, or char, or an enum/tagged-union type", describeType(snapshot, subjectNode.Type), wantName(width))
	}
	if err != nil {
		return "", err
	}
	// Group case nodes by shared body node ID to detect multi-value case
	// labels (a `case 1, 2:` clause produces two SwitchCase nodes sharing
	// one body node ID). Preserve encounter order within each group and
	// across groups.
	type caseGroup struct {
		bodyID  tir.NodeID
		caseIDs []tir.NodeID
		elseID  tir.NodeID // non-zero if this group is the else/default arm
	}
	// Use a slice to preserve encounter order; a map for O(1) body-to-group
	// lookup.
	groupByBody := make(map[tir.NodeID]int)
	var groups []caseGroup
	for _, caseID := range switchNode.Children[1:] {
		caseNode, ok := unit.Node(caseID)
		if !ok {
			return "", fmt.Errorf("switch statement references invalid case node %d", caseID)
		}
		if caseNode.Kind != tir.SwitchCase {
			return "", fmt.Errorf("switch statement child is a %s, want a SwitchCase", caseNode.Kind)
		}
		if caseNode.HasElse {
			// The else/default arm has no Literal and no CaseValue.
			groups = append(groups, caseGroup{bodyID: caseNode.Children[0], elseID: caseID})
			continue
		}
		if caseNode.CaseValue != 0 {
			// An enum-variant case: its CaseValue is the variant symbol, which
			// becomes the C enum constant label. It requires an enum-typed
			// subject (the checker only produces CaseValue cases for an
			// enum/tagged-union subject — confirmed against a real fixture),
			// and the variant must be one of the subject enum's declared
			// variants.
			if enumSubject == 0 {
				return "", fmt.Errorf("switch case references enum variant symbol %d, but the subject is not an enum or tagged-union type", caseNode.CaseValue)
			}
			if !containsVariant(enumVariants, caseNode.CaseValue) {
				return "", fmt.Errorf("switch case references variant symbol %d, which is not one of the subject enum %s's declared variants", caseNode.CaseValue, enumTypeName(enumSubject))
			}
		}
		// Case body node: a SwitchCase with 1 child has the body directly as
		// Children[0]; with 2 children the body is still Children[0] (the
		// second is unused defense — the body block arrives as the direct
		// child, confirmed against real fixtures).
		var bodyID tir.NodeID
		if len(caseNode.Children) == 1 {
			bodyID = caseNode.Children[0]
		} else if len(caseNode.Children) == 2 {
			bodyID = caseNode.Children[0]
		} else {
			return "", fmt.Errorf("switch case has %d child(ren), want 1 or 2 (the body block)", len(caseNode.Children))
		}
		if idx, exists := groupByBody[bodyID]; exists {
			groups[idx].caseIDs = append(groups[idx].caseIDs, caseID)
		} else {
			idx := len(groups)
			groupByBody[bodyID] = idx
			groups = append(groups, caseGroup{bodyID: bodyID, caseIDs: []tir.NodeID{caseID}})
		}
	}
	indent := strings.Repeat("    ", depth+1)
	caseIndent := strings.Repeat("    ", depth+2)
	// A fall-through switch's case body is emitted with a trailing `break;`
	// inside its braces: in C a case body falls through into the next case
	// unless it ends in a jump, and a Pebble case body may simply fall through
	// off its last statement. The break is emitted unconditionally — after a
	// body that already ends in a return/break/continue it is unreachable C,
	// which is valid and warning-free under the mandated -Wall -Wextra -Werror
	// (no -Wunreachable-code), and it is what makes a body that does NOT end in
	// a jump terminate its case instead of leaking into the next case's
	// statements. The tail-position switch never needs it, because every case
	// body ends in a return (see buildSwitchCaseBody).
	bodyWrap := func(bodyText string) string {
		if fallThrough {
			return fmt.Sprintf("{\n%s\n%sbreak;\n%s}", bodyText, caseIndent+"    ", caseIndent)
		}
		return fmt.Sprintf("{\n%s\n%s}", bodyText, caseIndent)
	}
	var parts []string
	for _, g := range groups {
		if g.elseID != 0 {
			// The else/default arm.
			bodyText, err := buildSwitchCaseBodyOrFallthrough(unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%sdefault: %s", caseIndent, bodyWrap(bodyText)))
			continue
		}
		// Emit stacked case labels for each SwitchCase in the group.
		for _, caseID := range g.caseIDs {
			caseNode, _ := unit.Node(caseID)
			label, err := buildCaseLabel(snapshot, caseNode, width)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s%s", caseIndent, label))
		}
		// The body is shared across all cases in the group.
		bodyText, err := buildSwitchCaseBodyOrFallthrough(unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
		if err != nil {
			return "", err
		}
		parts = append(parts, fmt.Sprintf("%s%s", caseIndent, bodyWrap(bodyText)))
	}
	return fmt.Sprintf("%sswitch (%s) {\n%s\n%s}", indent, subjectExpr, strings.Join(parts, "\n"), indent), nil
}

// buildSwitchCaseBodyOrFallthrough selects the case-body builder for one
// switch case by the switch's position: a tail-position switch (fallthrough
// false) builds the body under buildSwitchCaseBody's every-arm-must-return
// grammar; a fall-through switch (fallthrough true) builds it under
// buildLoopSwitchCaseBody's may-fall-through grammar.
func buildSwitchCaseBodyOrFallthrough(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if fallThrough {
		return buildLoopSwitchCaseBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	return buildSwitchCaseBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
}

// buildCaseLabel emits one C `case <value>:` label from a SwitchCase node.
// An enum-variant case (CaseValue set — a CaseValue-based case, produced by
// the checker for an enum subject) is emitted as
// `case pebble_variant_<caseValue>:`, the variant's C enum constant, whose
// value (the variant's ordinal in the enum's declared order) matches the
// subject's own typedef by construction. An integer literal is emitted as its
// decimal text; a bool literal is emitted as `0` (false) or `1` (true), since
// C treats bool as an integer type and switch cases require integral constant
// expressions; a char literal is emitted as `case (int32_t)<scalar>:`, the
// same int32_t spelling buildCharOperand gives a char value everywhere, so
// the label matches a char-typed subject's integral C representation. Any
// other case shape is a clean rejection.
func buildCaseLabel(snapshot *types.Snapshot, caseNode tir.Node, width types.BuiltinKind) (string, error) {
	if caseNode.CaseValue != 0 {
		// An enum-variant case label, emitted as the variant's C enum constant
		// name. buildSwitch has already verified the subject is a plain enum
		// and the variant belongs to it; this function only spells the label.
		return "case " + enumVariantName(caseNode.CaseValue) + ":", nil
	}
	switch caseNode.Literal.Kind {
	case tir.LiteralChar:
		// A char case label, emitted as the char's int32_t scalar value, the
		// same C representation buildCharOperand gives a char literal (see
		// buildCharLiteralValue). C switch case labels require integral
		// constant expressions, so the cast of the scalar literal is exactly
		// right.
		valueText, err := buildCharLiteralValue(caseNode)
		if err != nil {
			return "", err
		}
		return "case " + valueText + ":", nil
	case tir.LiteralInteger:
		text := caseNode.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("switch case contains an integer literal with malformed text %q", text)
		}
		litWidth, _ := resolvedBuiltin(snapshot, caseNode.Type)
		return "case " + integerLiteralText(text, litWidth) + ":", nil
	case tir.LiteralBool:
		if caseNode.Literal.Bool {
			return "case 1:", nil
		}
		return "case 0:", nil
	default:
		return "", fmt.Errorf("switch case has literal kind %s, want an integer, bool, or char constant", caseNode.Literal.Kind)
	}
}

// buildSwitchCaseBody builds the C text for a switch case's body. The body may
// be a Block node (a multi-statement case body) or a bare statement node (a
// single-statement case body). A Block body is built via buildBlock at the next
// nesting depth — the same recursive block grammar every other block in this
// backend uses — so an arm may contain its own locals, reassignments, nested
// if/else, and loops. A bare statement body is built directly: the only
// supported bare statement is a Return (the case body must end in a return),
// built at the next nesting depth with the same expression grammar buildBlock's
// tail return uses.
func buildSwitchCaseBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildBlock(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	// Bare single-statement case body: must be a Return.
	if bodyNode.Kind == tir.Return {
		if len(bodyNode.Children) != 1 {
			return "", fmt.Errorf("switch case bare return statement has %d argument(s), want exactly one expression", len(bodyNode.Children))
		}
		indent := strings.Repeat("    ", depth+1)
		var returnValue string
		var err error
		var preReturn string
		if result.isChar {
			// A char-returning function's bare single-statement case body
			// returning a char value: built under the char grammar by
			// buildCharOperand, exactly like buildBlock's tail-position Return
			// case.
			returnValue, err = buildCharOperand(unit, snapshot, fileSet, bodyNode.Children[0], locals, width)
		} else if result.isStr {
			returnValue, err = buildStrOperand(unit, snapshot, fileSet, bodyNode.Children[0], locals, width)
		} else if result.tuple != 0 || result.structType != 0 {
			preReturn, returnValue, err = buildAggregateReturnValue(unit, snapshot, fileSet, bodyNode.Children[0], locals, result, indent, width)
		} else if result.sliceType != 0 {
			// A slice-returning function's bare single-statement case body
			// returning a fresh slice construction: the construction needs the
			// two-statement temp-then-construction shape (see
			// buildSliceReturnValue), so the temp declaration is returned
			// separately and joined into the case body ahead of the final
			// return line, the same mechanical shape the deferred statements
			// below demonstrate.
			preReturn, returnValue, err = buildSliceReturnValue(unit, snapshot, fileSet, bodyNode.Children[0], locals, result, indent, width)
		} else if result.kind == types.F32 || result.kind == types.F64 {
			// A float-returning entry's bare single-statement case body
			// returning a float value: built under the float grammar by
			// buildFloatExpr, exactly like buildBlock's tail-position Return
			// case.
			returnValue, err = buildFloatExpr(unit, snapshot, fileSet, bodyNode.Children[0], locals, result.kind)
		} else {
			returnValue, err = buildExpr(unit, snapshot, fileSet, bodyNode.Children[0], locals, width, width)
		}
		if err != nil {
			return "", err
		}
		deferText, err := buildDeferredStatements(unit, snapshot, fileSet, bodyNode.DeferChain, locals, indent, "switch case body", width, unions)
		if err != nil {
			return "", err
		}
		parts := []string{}
		if preReturn != "" {
			parts = append(parts, preReturn)
		}
		if deferText != "" {
			parts = append(parts, deferText)
		}
		parts = append(parts, indent+"return "+returnValue+";")
		return strings.Join(parts, "\n"), nil
	}
	return "", fmt.Errorf("switch case body is a %s, want a Block or a Return", bodyNode.Kind)
}

// buildLoopSwitchCaseBody builds the C text for one case body of a
// fall-through switch (see buildLoopSwitch): the case-body twin of
// buildSwitchCaseBody whose only difference is that the body is an ordinary
// fall-through statement sequence rather than one that must end in a return.
// The body may be a Block node (a multi-statement case body) or a bare
// statement node (a single-statement case body with no braces). A Block body
// is built via buildFallthroughBody — the same shared "arbitrary statement
// sequence, no forced tail" builder a fall-through if's arms use, so a case
// body may declare locals, reassign enclosing ones, print, call void helpers,
// nest ifs/switches/loops, return early, or fall through off the end — and a
// bare statement body is built directly via buildFallthroughStatement. Both
// are the exact same dispatch a loop body's statements go through, so a break
// or continue inside a case body resolves (by Pebble's own control-flow
// rules) to the nearest enclosing loop or switch and is emitted as the
// equivalent C jump. The caller (buildSwitchStatement) wraps the returned
// text with a trailing `break;` inside the case's braces, since a C case body
// that does not itself end in a jump must break to avoid leaking into the
// next case (see bodyWrap there).
func buildLoopSwitchCaseBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildFallthroughBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "switch case body")
	}
	// Bare single-statement case body: built as one fall-through statement by
	// the same dispatch a statement inside a Block case body (or a loop body,
	// or an if arm) goes through, so it may be a Store, a call, a print, a
	// return, a nested if/switch, and so on — not just a Return.
	indent := strings.Repeat("    ", depth+1)
	return buildFallthroughStatement(unit, snapshot, fileSet, bodyID, locals, indent, depth, width, result, unions, "switch case body")
}

// buildIf validates and builds the C text for a two-armed if/else block: a
// tir.If with HasElse set, whose condition is a direct integer comparison
// (buildComparison) and whose two arms are Blocks built by recursing into
// buildBlock at the next nesting depth, each receiving the same locals set the
// enclosing block threads in (buildBlock copies it per arm, so the two arms
// never see each other's declarations). The emitted text is indented at this
// block's depth:
//
//	<indent>if (<condition>) {
//	<arm body, one level deeper>
//	<indent>} else {
//	<arm body, one level deeper>
//	<indent>}
//
// Any other shape — an If without an else, an arm that is not a Block, or a
// block with the wrong child count — is a clean rejection naming what was
// found.
func buildIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if !ifNode.HasElse {
		return "", fmt.Errorf("entry function body ends with an if without an else; this backend only supports the two-armed if/else whose arms each end in one return, found an if with no else")
	}
	if len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body ends with an if with %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildBlock(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	elseText, err := buildBlock(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildWhile validates and builds the C text for a while loop statement: a
// tir.While with exactly two children — Children[0] the condition, a direct
// integer comparison built by buildComparison (the same six operators and the
// same int-literal-in-condition handling an if condition uses), and Children[1]
// the loop body, which must be a Block built by buildLoopBody at the next
// nesting depth. The loop body is its own scope: buildLoopBody clones the
// incoming locals before extending them, so a local declared inside the loop is
// invisible outside it and re-initializes each C iteration (correct C
// block-scope behavior). The emitted text is indented at this block's depth,
// mirroring buildIf exactly:
//
//	<indent>while (<condition>) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// Any other shape — a wrong child count, or a body that is not a Block — is a
// clean rejection naming what was found.
func buildWhile(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, whileNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(whileNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop has %d child(ren), want exactly 2 (the condition, then the loop body)", len(whileNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, whileNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, whileNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%swhile (%s) {\n%s\n%s}", indent, condition, bodyText, indent), nil
}

// buildRangeLoop validates and builds the C text for a range loop statement: a
// tir.RangeLoop with exactly three children — Children[0] the start value,
// Children[1] the end value, and Children[2] the loop body, which must be a
// Block built by buildLoopBody at the next nesting depth — and a bound
// iterator (`loop start..end : name { ... }`) whose own symbol.SymbolID is
// recorded on the node's Symbol field. The loop lowers directly to a C for
// loop whose loop counter IS the iterator, the representation this backend's
// design decides:
//
//	<indent>for (int32_t pebble_local_<iterSym> = <start>; pebble_local_<iterSym> < <end>; pebble_local_<iterSym>++) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// `<` for the exclusive form (`..`), `<=` for the inclusive form (`..=`),
// from the node's RangeInclusive field. When the end bound is not an integer
// literal, a `pebble_temp_<endNodeID>` C local holding the end value is
// declared before the loop (at the same indent, as the loop's own leading
// statement) and the condition compares against that local instead of
// re-splicing the raw end expression — so a side-effecting or expensive end
// bound is evaluated exactly once, not once per condition check. A literal end
// bound is spliced directly (re-splicing a decimal number has no
// evaluation-order consequence). The iterator's own C type is the entry's
// resolved width (cType(width)); the start/end are ordinary integer
// expressions built by buildRangeBound (an int-typed integer literal lowered
// as its decimal text, anything else via buildExpr at the entry's width — the
// checker leaves the bounds as the unanchored int builtin whenever the
// iterator is never used in a width-anchoring position, confirmed against a
// real fixture). Before the body is built, the loop's own locals scope is
// seeded with the iterator (scope[iteratorSymbol] = localInfo{kind: width}),
// exactly the seeding pattern a helper's parameters already use, so a
// SymbolValue reference to the iterator inside the body resolves through the
// existing machinery unchanged. The loop body is built by the exact same
// buildLoopBody buildWhile uses, one level deeper; the body is its own scope
// (buildLoopBody clones), so nothing the body declares leaks outside, while
// the seeded iterator remains visible inside. break/continue inside the body
// are handled by buildLoopBody's own Break/Continue cases — plain C
// break/continue already target the nearest enclosing loop, which the emitted
// for loop is (confirmed against a real fixture that a Break/Continue inside
// a range loop body names the range loop's own Region as its Target). Any
// other shape — a wrong child count, an unbound loop (`loop start..end {
// ... }` with no `: name`, which has no way to be observed from inside and is
// low-value), or a body that is not a Block — is a clean rejection naming
// what was found.
func buildRangeLoop(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, rangeNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(rangeNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block range loop has %d child(ren), want exactly 3 (the start value, the end value, then the loop body)", len(rangeNode.Children))
	}
	if rangeNode.Symbol == 0 {
		// The unbound form (`loop start..end { ... }`, no `: name`) carries no
		// iterator symbol on the node (Symbol stays zero — confirmed against a
		// real fixture) and nothing for the body to read the loop's current
		// value from, so it is low-value and rejected cleanly rather than
		// lowered with a synthetic counter the source never names.
		return "", fmt.Errorf("entry function body block contains an unbound range loop (loop start..end { ... } with no `: name` iterator); only the bound `loop start..end : name { ... }` form is supported")
	}
	startText, err := buildRangeBound(unit, snapshot, fileSet, rangeNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	endText, err := buildRangeBound(unit, snapshot, fileSet, rangeNode.Children[1], locals, width)
	if err != nil {
		return "", err
	}
	// The loop's own scope is a clone of the enclosing set seeded with the
	// iterator as an ordinary local of the loop's bound type — the same
	// seeding pattern a helper's parameters use — so a SymbolValue reference
	// to the iterator inside the body (and a Store reassigning it, were the
	// checker to permit one) resolves through the existing machinery with zero
	// changes to buildExpr. The clone discipline keeps the iterator and
	// anything the body declares out of this block's own scope map. The
	// iterator's type is the bounds' own type, not necessarily the entry's
	// width: a range loop's two bounds always share one concrete integer type
	// (the checker anchors the start bound to the end bound's type), and when
	// that type is uint (std/hmap.peb's `loop 0..new_cap : i { ... }` over a
	// uint capacity) the iterator must be a uint64_t declared at uint's own C
	// type — declaring it at the entry width and comparing it against a
	// uint64_t bound would trip -Wsign-compare under the mandated -Wall
	// -Wextra -Werror. The body's references to the iterator then resolve at
	// that same type (uint routes through buildUintExpr, any other
	// non-entry-width integer through buildExpr at its own width).
	boundType := width
	if startNode, startOK := unit.Node(rangeNode.Children[0]); startOK {
		if resolved, integer := resolvedBuiltin(snapshot, startNode.Type); integer && cType(resolved) != "" {
			boundType = resolved
		}
	}
	loopScope := cloneLocals(locals)
	loopScope[rangeNode.Symbol] = localInfo{kind: boundType}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, rangeNode.Children[2], loopScope, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	// Determine loop direction. When both bounds are compile-time integer
	// literals (their buildRangeBound text is a plain decimal), compare them
	// statically to choose the comparison operator and step direction. This
	// handles descending ranges correctly (start > end emits `>` and `--`
	// instead of the old unconditional `<` and `++` which caused zero
	// iterations). Non-literal bounds (variables, expressions, function calls)
	// keep the existing ascending behavior unchanged.
	startVal, startIsLiteral := strconv.Atoi(startText)
	endVal, endIsLiteral := strconv.Atoi(endText)
	var rangeOp, step string
	if startIsLiteral == nil && endIsLiteral == nil && startVal > endVal {
		// Descending range: condition checks `>` (or `>=` for inclusive),
		// step decrements.
		rangeOp = ">"
		if rangeNode.RangeInclusive {
			rangeOp = ">="
		}
		step = "--"
	} else {
		// Ascending, zero-length, or non-literal bounds: condition checks `<`
		// (or `<=` for inclusive), step increments.
		rangeOp = "<"
		if rangeNode.RangeInclusive {
			rangeOp = "<="
		}
		step = "++"
	}
	indent := strings.Repeat("    ", depth+1)
	// A non-literal end bound is evaluated exactly once, into its own C local
	// declared before the loop, rather than spliced directly into the for-loop
	// condition (where ordinary C for semantics would re-evaluate it before
	// every iteration — a side-effecting or expensive end expression would run
	// once per iteration check instead of once total). The start bound needs no
	// such treatment: it is assigned into the C loop-variable initializer,
	// which C evaluates exactly once already. A plain integer literal end bound
	// keeps the existing fast path (re-splicing a decimal number has no
	// evaluation-order consequence), mirroring the literal/non-literal split
	// the descending-range direction logic above already makes. The temp's C
	// type is the loop's own bound type — the checker anchors the start bound
	// to the end bound's type, so boundType is the end expression's type too
	// and the cached local compares against the iterator without any new
	// signedness or width concern.
	endExpr := endText
	var endPre string
	if endIsLiteral != nil {
		endTemp := fmt.Sprintf("pebble_temp_%d", rangeNode.Children[1])
		endPre = fmt.Sprintf("%s%s %s = %s;", indent, cType(boundType), endTemp, endText)
		endExpr = endTemp
	}
	forText := fmt.Sprintf("%sfor (%s pebble_local_%d = %s; pebble_local_%d %s %s; pebble_local_%d%s) {\n%s\n%s}", indent, cType(boundType), rangeNode.Symbol, startText, rangeNode.Symbol, rangeOp, endExpr, rangeNode.Symbol, step, bodyText, indent)
	if endPre != "" {
		return endPre + "\n" + forText, nil
	}
	return forText, nil
}

// buildRangeBound builds one bound (the start or the end) of a range loop. A
// bound is an ordinary integer expression built by buildExpr at the entry's
// resolved width — a literal, a local reference, arithmetic, a helper call,
// anything buildExpr already builds. The one exception mirrors
// buildComparisonOperand: when the range loop's iterator is never used in a
// width-anchoring position, the checker leaves the bounds as the snapshot's
// unanchored int builtin (confirmed against real fixtures — `loop 0..3 : i {
// if i == 2 { ... } }` has int-typed bounds while `loop 0..3 : i { sum = sum
// + i; }` anchors both to i32), so an int-typed integer literal is lowered
// directly as its decimal text and an int-typed SymbolValue — only ever a
// range-loop iterator in this backend's grammar, e.g. a nested loop whose
// bound reads the outer loop's iterator — as its pebble_local_<symbol> name
// (the iterator is always declared in C at the entry's width, so the name is
// the correct C lvalue; assigning `0` into the iterator's int32_t and
// comparing it against `3` is trivially valid C). An int-typed non-literal
// bound — int-typed arithmetic from a loop whose iterator is never used —
// reaches buildExpr and is rejected there by its width gate, exactly as the
// rest of this backend treats int-typed arithmetic.
func buildRangeBound(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body block range loop references invalid bound node %d", id)
	}
	if node.Kind == tir.IntegerLiteral && node.Type == snapshot.Builtins().Int {
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body block range loop bound contains an integer literal with malformed text %q", text)
		}
		return text, nil
	}
	if node.Kind == tir.SymbolValue && node.Type == snapshot.Builtins().Int {
		if _, declared := locals[node.Symbol]; !declared {
			return "", fmt.Errorf("entry function body block range loop bound references symbol %d, which is not a local in scope", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	}
	// Any other bound is built at its OWN resolved integer width rather than
	// the ambient entry width, exactly as buildComparisonOperand and
	// buildCallArgument resolve their operands: a range loop's two bounds
	// always share one concrete integer type (the checker anchors the start
	// bound to the end bound's type), and that type need not be the entry's
	// width — std/hmap.peb's rehash/with_capacity loop over a uint-typed
	// capacity (`loop 0..new_cap : i { ... }`), where the checker anchors
	// the `0` start bound to uint, so the literal reaches this path as a
	// uint-typed IntegerLiteral that buildExpr's entry-width gate would
	// reject. uint is deliberately routed through buildUintExpr (the
	// dedicated uint grammar), excluded from the general buildExpr path the
	// same way buildCallArgument/buildComparisonOperand exclude it; any
	// other non-entry-width integer bound flows through buildExpr at the
	// bound's own resolved width.
	boundWidth, integerBound := resolvedBuiltin(snapshot, node.Type)
	if integerBound && cType(boundWidth) != "" && !isUint(snapshot, node.Type) {
		return buildExpr(unit, snapshot, fileSet, id, locals, boundWidth, width)
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(unit, snapshot, fileSet, id, locals, width, width)
}

// buildFor validates and builds the C text for a classic for loop statement
// (a tir.For): `for <init>; <cond>; <update> { <body> }` with each of the
// three clauses individually optional, lowering directly to a C for loop with
// the same three clauses:
//
//	<indent>for (<init>; <cond>; <update>) {
//	<loop body statements, one level deeper>
//	<indent>}
//
// For.Children is a variable-length list the checker builds by appending, in
// this fixed relative order, only the clauses actually present — the
// initializer, then the condition, then the update — with the body a Block
// always last (confirmed against real fixtures for every clause-presence
// combination). The clauses are disambiguated by their node categories, not
// by position assumptions: the condition, when present, is the unique
// CategoryValue child among the non-body children, and every other clause
// child is a CategoryNonvalue. The initializer, when present, must be a
// single Initialize declaring a local of the entry's width or bool (built by
// buildForInitClause) — a Store/CompoundStore/ExpressionStatement initializer
// is reachable from real source (`for step = 0; ...`, `for x += 1; ...`,
// `for x + 1; ...`) and is a clean rejection, matching the backend's rule
// that only an Initialize declares a local. The update, when present, must be
// a single Store reassigning a local already in scope or a single CompoundStore
// (a compound assignment such as `step += 1` or a postfix `step++`, built by
// buildForUpdateClause through buildStoreCore / buildCompoundStore) — a
// discarded-expression update (`for x + 1; ...`) is reachable from real source
// and is a clean rejection. With no condition present the checker's fixed relative
// order leaves at most `[initializer?] [update?]`, so an Initialize child is
// the initializer and a Store/CompoundStore child is the update; a lone
// no-condition Store is treated as the update (the in-scope update-only shape
// `for ; ; update {
// ... }`) — note this makes a no-condition Store *initializer* (`for step =
// 0;; { ... }`, out of scope but reachable) structurally indistinguishable
// from update-only and silently lowered as the update, a real ambiguity with
// no IR-level way to tell them apart (the For node carries only Region and
// Children). The condition is built by the exact same buildCondition an
// if/while condition uses. The body is built by the exact same buildLoopBody
// a while/range loop uses, one level deeper, against a cloned scope seeded
// with the initializer's local if it declares one — so a SymbolValue
// reference to the initializer's local inside the condition, update, or body
// resolves through the existing machinery unchanged, mirroring how a range
// loop seeds its iterator. If the initializer declares a local, a `(void)
// pebble_local_<symbol>;` cast is emitted as the body's first statement, the
// same -Wunused-variable defense every declared local gets (confirmed: cc
// fires -Wunused-variable under -Wall -Wextra -Werror for a for-init local
// never referenced anywhere, and the cast is a no-op when it is). The body is
// its own scope (buildLoopBody clones), so nothing the body declares leaks
// outside, while the seeded initializer local remains visible inside.
// break/continue inside the body are handled by buildLoopBody's own
// Break/Continue cases — plain C break/continue already target the nearest
// enclosing loop, which the emitted for loop is (the same confirmation a
// range loop made). Any other shape — an ambiguous clause list, an
// out-of-scope initializer or update, a missing or non-Block body — is a
// clean rejection naming what was found.
func buildFor(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, forNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(forNode.Children) < 1 || len(forNode.Children) > 4 {
		return "", fmt.Errorf("entry function body block for loop has %d child(ren), want 1 to 4 (the optional initializer, condition, and update clauses, then the loop body)", len(forNode.Children))
	}
	bodyID := forNode.Children[len(forNode.Children)-1]
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("entry function body block for loop references invalid body node %d", bodyID)
	}
	if bodyNode.Kind != tir.Block {
		return "", fmt.Errorf("entry function body block for loop body is a %s, want a Block", bodyNode.Kind)
	}
	clauses := forNode.Children[:len(forNode.Children)-1]
	// The condition, when present, is the unique CategoryValue child among the
	// non-body clauses (confirmed against real fixtures for every
	// clause-presence combination). Every other clause child is a
	// CategoryNonvalue (an Initialize/Store/CompoundStore/ExpressionStatement),
	// so a unique CategoryValue can only be the condition, and it appears in
	// the position matching its role: the checker appends clauses in the fixed
	// relative order initializer, condition, update.
	condIndex := -1
	for i, clauseID := range clauses {
		clause, clauseOK := unit.Node(clauseID)
		if !clauseOK {
			return "", fmt.Errorf("entry function body block for loop references invalid clause node %d", clauseID)
		}
		category, categoryOK := tir.CategoryOf(clause.Kind)
		if !categoryOK {
			return "", fmt.Errorf("entry function body block for loop clause %d has unknown node kind %s", i, clause.Kind)
		}
		if category == tir.CategoryValue {
			if condIndex >= 0 {
				return "", fmt.Errorf("entry function body block for loop has %d value clause(s), want at most one (the condition)", len(clauses))
			}
			condIndex = i
		}
	}
	loopScope := cloneLocals(locals)
	var initText, initPre, condText, updateText, updatePre string
	var initSymbol symbol.SymbolID
	var updateID tir.NodeID
	if condIndex >= 0 {
		// The condition is present. The initializer slot is the at-most-one
		// nonvalue clause before it, and the update slot the at-most-one
		// nonvalue clause after it. A nonvalue clause before the condition is
		// the initializer and must be an Initialize; a nonvalue clause after
		// it is the update and must be a Store.
		if condIndex > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) before its condition, want at most one (the initializer)", condIndex)
		}
		if len(clauses)-condIndex-1 > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) after its condition, want at most one (the update)", len(clauses)-condIndex-1)
		}
		if condIndex == 1 {
			pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
		}
		cond, err := buildCondition(unit, snapshot, fileSet, clauses[condIndex], loopScope, width)
		if err != nil {
			return "", err
		}
		condText = cond
		if len(clauses)-condIndex-1 == 1 {
			updateID = clauses[len(clauses)-1]
			pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[len(clauses)-1], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			updateText = text
			updatePre = pre
		}
	} else {
		// No condition: the checker's fixed relative order leaves at most
		// `[initializer?] [update?]`, so an Initialize child is the
		// initializer and a Store child is the update.
		switch len(clauses) {
		case 0:
			// All three clauses absent — an infinite loop from the header's
			// perspective (`for (; ; )`).
		case 1:
			clause, _ := unit.Node(clauses[0])
			switch clause.Kind {
			case tir.Initialize:
				pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
				if err != nil {
					return "", err
				}
				initText = text
				initPre = pre
				initSymbol = symbol
			case tir.Store:
				// A lone no-condition Store is the update-only shape
				// (`for ; ; update { ... }`), the in-scope interpretation.
				// A no-condition Store *initializer* (`for step = 0;; { ...
				// }`) is structurally indistinguishable from it (confirmed
				// against real fixtures) and is documented as a real
				// ambiguity: it lowers as the update, never guessed as
				// something else. A lone no-condition CompoundStore is the
				// same update-only shape for a compound assignment or postfix
				// increment (`for ; ; i++ { ... }`).
				updateID = clauses[0]
				pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
				if err != nil {
					return "", err
				}
				updateText = text
				updatePre = pre
			case tir.CompoundStore:
				updateID = clauses[0]
				pre, text, err := buildForUpdateClause(unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
				if err != nil {
					return "", err
				}
				updateText = text
				updatePre = pre
			default:
				return "", fmt.Errorf("entry function body block for loop with no condition has a %s clause, want an Initialize (a local declaration), a Store (an update), or a CompoundStore (an update)", clause.Kind)
			}
		case 2:
			initClause, _ := unit.Node(clauses[0])
			updateClause, _ := unit.Node(clauses[1])
			if initClause.Kind != tir.Initialize {
				return "", fmt.Errorf("entry function body block for loop with no condition leads with a %s clause, want an Initialize (the initializer declares a local)", initClause.Kind)
			}
			if updateClause.Kind != tir.Store && updateClause.Kind != tir.CompoundStore {
				return "", fmt.Errorf("entry function body block for loop with no condition follows the initializer with a %s clause, want a Store or CompoundStore (the update)", updateClause.Kind)
			}
			pre, text, symbol, err := buildForInitClause(unit, snapshot, fileSet, clauses[0], loopScope, width)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
			updateID = clauses[1]
			pre, text, err = buildForUpdateClause(unit, snapshot, fileSet, clauses[1], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			updateText = text
			updatePre = pre
		default:
			return "", fmt.Errorf("entry function body block for loop with no condition has %d clause(s), want at most two (an initializer and an update)", len(clauses))
		}
	}
	bodyText, err := buildLoopBody(unit, snapshot, fileSet, bodyID, loopScope, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	if initSymbol != 0 {
		// The initializer declared a local whose C declaration lives in the
		// for-header, so the (void) cast against -Wunused-variable must go
		// inside the loop body — the for-init local's C scope is the loop
		// itself, and cc fires the warning for a for-init local never
		// referenced anywhere (confirmed with a real compile). The cast is the
		// body's first statement, at the body's own statement indent.
		bodyIndent := strings.Repeat("    ", depth+2)
		bodyText = bodyIndent + fmt.Sprintf("(void)pebble_local_%d;", initSymbol) + "\n" + bodyText
	}
	indent := strings.Repeat("    ", depth+1)
	forText := fmt.Sprintf("%sfor (%s; %s; %s) {\n%s\n%s}", indent, initText, condText, updateText, bodyText, indent)
	var preLines string
	if initPre != "" {
		// The initializer's source-integer temp lives in the enclosing block,
		// not the for-header (a header clause is a single C declaration, and
		// the int64_t temp and the optional-typed local have different C
		// types), so it is emitted as a statement before the for — the same
		// leading position updatePre uses.
		preLines += indent + initPre + "\n"
	}
	if updatePre != "" {
		updateNode, ok := unit.Node(updateID)
		if !ok || len(updateNode.Children) == 0 {
			return "", fmt.Errorf("entry function body block for loop update references invalid compound place")
		}
		lvalue, _, err := buildPlaceLValue(unit, snapshot, fileSet, updateNode.Children[0], loopScope, width)
		if err != nil {
			return "", err
		}
		tempName := fmt.Sprintf("pebble_compound_ptr_%d", updateID)
		updatePre = fmt.Sprintf("%s *%s;", cType(width), tempName)
		updateText = fmt.Sprintf("%s = &(%s), %s", tempName, lvalue, updateText)
		forText = fmt.Sprintf("%sfor (%s; %s; %s) {\n%s\n%s}", indent, initText, condText, updateText, bodyText, indent)
		preLines += indent + updatePre + "\n"
	}
	if preLines != "" {
		return preLines + forText, nil
	}
	return forText, nil
}

// buildForInitClause validates and builds the C init-clause text for a classic
// for loop's initializer: `<cType> pebble_local_<symbol> = <expr>` — a C
// declaration with no leading indent, no statement-terminating newline, and no
// trailing `;` of its own (the for-header `for (<init>; <cond>; <update>)`'s
// first `;` is what terminates the init clause). The initializer must be a
// single Initialize (a local declaration) — a Store (an assignment), a
// CompoundStore, or a discarded ExpressionStatement initializer are all
// reachable from real source but out of scope and cleanly rejected, matching
// the backend's rule that only an Initialize declares a local. The declared
// local may be of any integer width (not just the entry's resolved width),
// bool, or char — the same scalar grammars a bare Initialize supports —
// validated and emitted by buildScalarInitializeCore, which also records the
// local in the caller's loop scope so the condition, update, and body can
// reference it. Returns the clause text and the declared symbol (so buildFor
// can emit the (void) cast as the body's first statement), plus, for an
// OptionalIntegerToEnum initializer, a pre statement text that must be emitted
// BEFORE the for statement — the source integer's one-time-evaluation temp
// declaration, which a single for-header declaration cannot hold alongside the
// optional-typed local (the two have different C types, and a for-header
// declaration is a single C declaration) — mirroring the updatePre mechanism
// buildCompoundStore uses.
func buildForInitClause(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, string, symbol.SymbolID, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer references invalid node %d", id)
	}
	if statement.Kind != tir.Initialize {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer is a %s, want a local declaration (an Initialize); a for-loop initializer must declare a local of %s or bool", statement.Kind, wantName(width))
	}
	if len(statement.Children) != 1 {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer initializes %d value(s), want exactly one expression", len(statement.Children))
	}
	if _, declared := scope[statement.Symbol]; declared {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer declares local %d more than once", statement.Symbol)
	}
	initValue, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer references invalid value node %d", statement.Children[0])
	}
	if initValue.Kind == tir.OptionalIntegerToEnum {
		// An integer-to-optional-enum cast as the for-loop initializer
		// (`for var c ?Color = 5 as ?Color; ...`): the cast is supported only
		// in a local-declaration initializer, and the for-loop initializer IS
		// a local declaration — but the header clause must be a single C
		// declaration, and the cast needs the source hoisted into an int64_t
		// temp of a different C type (see
		// buildOptionalIntegerToEnumDeclaration), so the temp's declaration is
		// returned as a pre statement buildFor emits before the for statement,
		// and the header clause is the optional-typed local's own declaration
		// reading that temp.
		pre, core, err := buildOptionalIntegerToEnumDeclaration(unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", id, width)
		if err != nil {
			return "", "", 0, err
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return pre, core, statement.Symbol, nil
	}
	pre, core, err := buildScalarInitializeCore(unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", width)
	if err != nil {
		return "", "", 0, err
	}
	return pre, core, statement.Symbol, nil
}

// buildForUpdateClause validates and builds the C update-clause text for a
// classic for loop's update: `pebble_local_<symbol> = <expr>` with no leading
// indent and no trailing `;` — the for statement's own header syntax supplies
// the semicolons, so the update clause is a bare assignment expression, not a
// full C statement (the equivalent block-level Store's trailing `;` is
// deliberately omitted). The update must be a single Store reassigning a
// local already in scope, validated and emitted by buildStoreCore against the
// local's own declared type (the entry's width or bool), or a single
// CompoundStore (a compound assignment such as `step += 1` or a postfix
// `step++`), validated and emitted by buildCompoundStore; a discarded-
// expression update (`for x + 1; ...`) is reachable from real source but out
// of scope and cleanly rejected.
func buildForUpdateClause(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, string, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body block for loop update references invalid node %d", id)
	}
	if statement.Kind != tir.Store && statement.Kind != tir.CompoundStore {
		return "", "", fmt.Errorf("entry function body block for loop update is a %s, want a Store (a reassignment of a local already in scope) or a CompoundStore (a compound assignment or postfix increment/decrement); a for-loop update must be a single assignment", statement.Kind)
	}
	if statement.Kind == tir.CompoundStore {
		return buildCompoundStore(unit, snapshot, fileSet, id, statement, scope, "entry function body block for loop update", width)
	}
	core, err := buildStoreCore(unit, snapshot, fileSet, statement, scope, "entry function body block for loop update", width, unions)
	return "", core, err
}

// buildLoopBody validates and builds the C statement sequence for a loop body
// (a while's, a range loop's, or a classic for loop's): a Block built by the
// shared buildFallthroughBody fall-through statement-sequence builder, with
// the loop-body context naming. This is the loop-specific entry point
// (buildWhile/buildRangeLoop/buildFor/buildLoopIf recurse here); the same
// builder serves a fall-through if's arms and a fall-through switch's case
// bodies uniformly, since they all need the identical "arbitrary statement
// sequence, no forced tail" capability. An empty loop body (zero children) is
// legal — `while cond {}` is a real, if useless, program — and emits no
// statements at all.
func buildLoopBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildFallthroughBody(unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "entry function body block while loop body")
}

// buildFallthroughBody validates and builds the C statement sequence for an
// ordinary, fall-through block: a Block whose children are local declarations
// (Initialize), reassignments (Store), print statements (Print), bare
// discarded-expression statements (ExpressionStatement), conditional if
// statements (a tir.If built by buildLoopIf — the else is optional), switch
// statements (a tir.Switch built by buildLoopSwitch — case bodies may fall
// through or return), nested while/range/classic-for loops (built by
// buildWhile/buildRangeLoop/buildFor), return statements (built by
// buildReturnStatement), and break/continue statements (a tir.Break /
// tir.Continue built by buildLoopJump), built one level deeper than the
// enclosing block. A fall-through block has no required tail — it just runs
// statements and does not need to end in a return or if — so buildBlock is
// deliberately not reused here; the grammar is genuinely different. This is
// the shared builder for every sequence with that shape: a loop body
// (buildLoopBody), a fall-through if's arm (buildLeadingIf and buildLoopIf),
// and a fall-through switch's case body (buildLoopSwitchCaseBody), all of
// which need the identical "arbitrary statement sequence, no forced tail"
// capability. The block is its own scope: locals are cloned from the
// enclosing set (the same cloneLocals discipline buildIf's arms use) before
// any declaration is added, so a local declared inside it is invisible
// outside it. A nested loop's body and each if arm/switch case body are their
// own scopes in turn (buildWhile/buildRangeLoop/buildFor/buildLoopIf and
// buildLoopSwitch all recurse into this same builder, which clones per
// entry), so a local declared inside one of them is invisible to its siblings
// and to anything outside it. A break or continue inside the sequence targets
// the nearest enclosing loop or switch by Pebble's own control-flow rules;
// the emitted C break/continue resolves to the same construct, so the
// translation is direct and correct (see buildLoopJump). Any other statement
// kind is a clean rejection naming what was found. An empty block (zero
// children) emits no statements at all. context names the enclosing construct
// in error messages.
func buildFallthroughBody(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	body, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("%s references invalid node %d", context, bodyID)
	}
	if body.Kind != tir.Block {
		return "", fmt.Errorf("%s is a %s, want a Block", context, body.Kind)
	}
	if len(body.Children) == 0 {
		return "", nil
	}
	scope := cloneLocals(locals)
	indent := strings.Repeat("    ", depth+1)
	var statements []string
	for _, childID := range body.Children {
		text, err := buildFallthroughStatement(unit, snapshot, fileSet, childID, scope, indent, depth, width, result, unions, context)
		if err != nil {
			return "", err
		}
		if text != "" {
			statements = append(statements, text)
		}
	}
	return strings.Join(statements, "\n"), nil
}

// buildFallthroughStatement validates and builds the C text for one statement
// in a fall-through statement sequence (see buildFallthroughBody), dispatching
// on the statement kind exactly as buildBlock's leading-statement loop and the
// loop-body statement switch do, but with the loop body's own "no required
// tail" grammar extended to the whole sequence: a nested while/range loop/for
// loop recurses into its own loop builder (which recurses back into
// buildFallthroughBody for its body), a conditional if is built by buildLoopIf
// (arms are themselves fall-through sequences, optional else), a switch by
// buildLoopSwitch (case bodies are themselves fall-through sequences), a
// return by buildReturnStatement, a break/continue by buildLoopJump, a
// DeferRegister emits nothing, and everything else — an Initialize, a Store,
// an ExpressionStatement, a Print — flows through the shared
// buildLeadingStatement. indent is the statement's C indentation (the
// sequence's own indent, depth+1 levels); depth is the sequence's nesting
// depth, passed to the nested control-flow builders unchanged so they open
// their own bodies one level deeper. context names the enclosing construct in
// error messages.
func buildFallthroughStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, childID tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	statement, ok := unit.Node(childID)
	if !ok {
		return "", fmt.Errorf("%s references invalid statement node %d", context, childID)
	}
	var text string
	var err error
	switch statement.Kind {
	case tir.While:
		// A nested while inside a fall-through sequence reuses buildWhile
		// unchanged: it already recurses into buildLoopBody (via
		// buildFallthroughBody) for its own body, so nested loops compose
		// without any change to buildWhile itself.
		text, err = buildWhile(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.RangeLoop:
		// A nested range loop inside a fall-through sequence (a loop body, an
		// if arm, or a switch case body) reuses buildRangeLoop unchanged: it
		// recurses into this same builder for its own body, so nested range
		// loops compose exactly like nested whiles do.
		text, err = buildRangeLoop(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.For:
		// A nested classic for loop inside a fall-through sequence reuses
		// buildFor unchanged: it recurses into this same builder for its own
		// body, so nested classic for loops compose exactly like nested whiles
		// and range loops do.
		text, err = buildFor(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.If:
		// A conditional statement inside a fall-through sequence is built by
		// buildLoopIf: its arms are themselves fall-through sequences (no
		// required tail, optional else), genuinely different from the
		// tail-requiring buildIf. Because buildLoopIf recurses into
		// buildLoopBody for each arm, a break or continue inside an arm is
		// handled by this same switch, unchanged.
		text, err = buildLoopIf(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Switch:
		// A switch statement inside a fall-through sequence is built by
		// buildLoopSwitch: its case bodies are themselves fall-through
		// sequences (may return or fall through), unlike the tail-requiring
		// buildSwitch. Because buildLoopSwitch recurses into this same
		// dispatch for each case body, a break or continue inside a case body
		// is handled here, unchanged — C's own break/continue scoping resolves
		// it to the nearest enclosing loop or switch, which matches Pebble's
		// break-target rules (see buildLoopSwitch).
		text, err = buildLoopSwitch(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Return:
		// A return inside a fall-through sequence — an if arm, a switch case
		// body, or a loop body — exits the enclosing function immediately,
		// built by the same shared buildReturnStatement the block-tail Return
		// case uses.
		text, err = buildReturnStatement(unit, snapshot, fileSet, statement, scope, indent, context, width, result, unions)
	case tir.Break:
		text, err = buildLoopJump(unit, snapshot, fileSet, statement, "break", indent, context, scope, width, unions)
	case tir.Continue:
		text, err = buildLoopJump(unit, snapshot, fileSet, statement, "continue", indent, context, scope, width, unions)
	case tir.DeferRegister:
		// A DeferRegister in a fall-through statement sequence is a
		// registration marker the checker's analysis already consumed; the
		// backend must emit nothing at this position. The deferred statement
		// is only ever emitted at exit points whose DeferChain references it.
		return "", nil
	case tir.ExpressionStatement:
		// A bare discarded-expression statement — `helper();` on its own line
		// — flows through the same shared leading-statement builder buildBlock
		// uses, so the emission logic lives in exactly one place. (The default
		// case below would reach buildLeadingStatement too; the case is
		// spelled out so this switch documents the supported kinds the way it
		// does for While/RangeLoop/For/If/Switch/Return/Break/Continue/
		// DeferRegister.)
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	case tir.Print:
		// A print statement — `print a, b;` on its own line — flows through
		// the same shared leading-statement builder buildBlock uses, so the
		// emission logic lives in exactly one place.
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	default:
		text, err = buildLeadingStatement(unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	}
	if err != nil {
		return "", err
	}
	return text, nil
}

// buildLoopIf validates and builds the C text for a conditional statement
// (tir.If) inside a while loop body. Unlike buildIf — which handles the
// two-armed, both-arms-return if/else a block must end with — a loop-body if
// is just a conditional statement: its arms are loop bodies (see buildLoopBody)
// with no required tail, and the else is optional. The child count is derived
// from HasElse (confirmed against a real fixture dump): a no-else If has
// exactly two children — the condition and the then-arm — and a HasElse If has
// exactly three — the condition, then-arm, and else-arm. The condition is a
// direct integer comparison built by buildComparison, exactly as buildIf and
// buildWhile use. Each arm is built by buildLoopBody at the next nesting depth,
// which clones the incoming locals per arm, so a local declared inside one arm
// is invisible to the sibling arm and to anything outside the if, while locals
// declared in the enclosing loop body remain visible inside both arms. The
// emitted text is indented at this statement's depth, mirroring buildIf:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>}
//
// or, with an else:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>} else {
//	<else statements, one level deeper>
//	<indent>}
//
// Any other shape — a child count inconsistent with HasElse, or an arm that is
// not a Block — is a clean rejection naming what was found.
func buildLoopIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block while loop body if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop body if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildLoopBody(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildLoopBody(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildLeadingIf validates and builds the C text for a conditional statement
// (tir.If) as an ordinary leading statement in a top-level function body —
// the non-tail, non-loop position, e.g. a guard clause `if x > 0 { return 1;
// }` followed by more statements. It is the top-level twin of buildLoopIf
// (which buildIf, the tail-requiring two-armed form, is NOT: a leading if
// need not be the block's last statement and its arms need not end in
// return). Exactly like buildLoopIf, the child count is derived from HasElse
// (a no-else If has exactly two children — the condition and the then-arm —
// and a HasElse If has exactly three), the condition is a direct integer
// comparison built by buildComparison, and the else is optional. The arms are
// built by buildFallthroughBody — the same shared "arbitrary statement
// sequence, no forced tail" builder a switch case body and a loop body use —
// at the next nesting depth, which clones the incoming locals per arm, so a
// local declared inside one arm is invisible to the sibling arm and to
// anything outside the if, while locals declared in the enclosing body remain
// visible inside both arms. The emitted text is indented at this statement's
// depth, mirroring buildIf and buildLoopIf:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>}
//
// or, with an else:
//
//	<indent>if (<condition>) {
//	<then statements, one level deeper>
//	<indent>} else {
//	<else statements, one level deeper>
//	<indent>}
//
// Any other shape — a child count inconsistent with HasElse, or an arm that is
// not a Block — is a clean rejection naming what was found. context names the
// enclosing construct in error messages and is prefixed with " arm" for each
// arm's own error messages.
func buildLeadingIf(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("%s if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", context, len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("%s if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", context, len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildFallthroughBody(unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions, context+" arm")
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildFallthroughBody(unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions, context+" arm")
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildDeferredStatements emits the C statements for each DeferRegister node in
// a DeferChain, in the chain's own order (already LIFO — last-registered-first
// — computed by the checker's deferChainFor in ir_builder_control.go). Each
// DeferRegister's single child is the deferred statement itself, built by the
// appropriate statement builder. The emitted text is a sequence of C statements
// at the given indent, joined by newlines, to be placed immediately before the
// actual exit statement (return/break/continue). The checker already rejects
// deferred return/break/continue/nested defer (C0613), so the only reachable
// deferred statement kinds from real source are Store (reassignment),
// CompoundStore (a compound assignment or postfix increment/decrement), Print
// (the built-in), and — since 10.33 — a bare discarded-expression statement
// that is a call to a void-returning function (defer helper();, built by the
// same buildExpressionStatement the leading-statement case uses). A
// DeferRegister whose child is an unsupported
// statement kind is a clean rejection naming what was found.
func buildDeferredStatements(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, chain []tir.NodeID, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	if len(chain) == 0 {
		return "", nil
	}
	var parts []string
	for _, deferRegID := range chain {
		deferReg, ok := unit.Node(deferRegID)
		if !ok {
			return "", fmt.Errorf("%s references invalid DeferRegister node %d", context, deferRegID)
		}
		if deferReg.Kind != tir.DeferRegister {
			return "", fmt.Errorf("%s DeferChain entry %d is a %s, want a DeferRegister", context, deferRegID, deferReg.Kind)
		}
		if len(deferReg.Children) != 1 {
			return "", fmt.Errorf("%s DeferRegister %d has %d child(ren), want exactly one (the deferred statement)", context, deferRegID, len(deferReg.Children))
		}
		stmt, ok := unit.Node(deferReg.Children[0])
		if !ok {
			return "", fmt.Errorf("%s DeferRegister %d references invalid statement child %d", context, deferRegID, deferReg.Children[0])
		}
		switch stmt.Kind {
		case tir.Store:
			core, err := buildStoreCore(unit, snapshot, fileSet, stmt, scope, context, width, unions)
			if err != nil {
				return "", err
			}
			parts = append(parts, indent+core+";")
		case tir.CompoundStore:
			// A deferred compound assignment or postfix increment/decrement —
			// `defer i += 1;` — built by the same shared buildCompoundStore a
			// non-deferred compound assignment uses, so the emission logic
			// lives in exactly one place.
			pre, core, err := buildCompoundStore(unit, snapshot, fileSet, deferReg.Children[0], stmt, scope, context, width)
			if err != nil {
				return "", err
			}
			if pre != "" {
				parts = append(parts, indent+pre)
			}
			parts = append(parts, indent+core+";")
		case tir.ExpressionStatement:
			// A deferred call to a void-returning function — `defer
			// helper();` — built by the same shared statement builder a
			// non-deferred discarded-expression statement uses, so the emission
			// logic lives in exactly one place. The builder rejects a deferred
			// call to a non-void-returning function (and any non-call discarded
			// expression) cleanly.
			text, err := buildExpressionStatement(unit, snapshot, fileSet, stmt, scope, indent, context, width)
			if err != nil {
				return "", err
			}
			parts = append(parts, text)
		case tir.Print:
			// A deferred print statement — `defer print a, b;` — built by the
			// same shared buildPrint a leading print statement uses, so the
			// emission logic lives in exactly one place. A slice-index
			// operand's temp declaration is returned as a leading pre-statement
			// at the same indent, the same mechanical shape the deferred
			// CompoundStore pre uses.
			pre, text, err := buildPrint(unit, snapshot, fileSet, stmt, scope, indent, context, width)
			if err != nil {
				return "", err
			}
			if pre != "" {
				parts = append(parts, pre)
			}
			parts = append(parts, text)
		case tir.Initialize:
			// A deferred local declaration is reachable from real source but
			// not yet supported as a deferred statement in this backend's
			// current scope. Reject cleanly rather than guess.
			return "", fmt.Errorf("%s deferred statement is an Initialize (local declaration), which is not supported as a deferred statement yet", context)
		default:
			return "", fmt.Errorf("%s deferred statement is a %s, which is not a supported deferred statement kind (only Store reassignment, a CompoundStore compound assignment or postfix increment/decrement, and a void-returning function call used as a statement are supported)", context, stmt.Kind)
		}
	}
	return strings.Join(parts, "\n"), nil
}

// buildLoopJump validates and builds the C text for one break/continue
// statement in a loop body. A tir.Break or tir.Continue is a leaf node (no
// children, confirmed against real fixtures) whose Target names the region of
// the loop the jump leaves, and whose DeferChain carries the DeferRegister
// nodes whose deferred statements must run before the jump. The chain is
// emitted as ordinary C statements (via buildDeferredStatements) immediately
// before the break/continue, in the chain's own LIFO order. The emitted C is
// the deferred statements followed by `break;` / `continue;` at the current
// indent: the language has no labeled break/continue, so a jump's Target
// always names the nearest enclosing loop and plain C break/continue — which
// already target the nearest enclosing loop by C's own scoping rules — is a
// direct, correct translation. No runtime helper is involved, and Target's
// value never needs to be consulted or compared; it is confirmed (against a
// nested-loop fixture) to name the loop that actually contains the jump, and
// the checker (C0611) already guarantees that loop is an enclosing one.
func buildLoopJump(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, keyword string, indent, context string, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, error) {
	deferText, err := buildDeferredStatements(unit, snapshot, fileSet, statement.DeferChain, scope, indent, context, width, unions)
	if err != nil {
		return "", err
	}
	jump := fmt.Sprintf("%s%s;", indent, keyword)
	if deferText == "" {
		return jump, nil
	}
	return deferText + "\n" + jump, nil
}

// buildLeadingStatement validates and builds one leading statement in the
// block grammar shared by buildBlock and buildFallthroughBody: an Initialize (a
// local declaration), a Store (a reassignment of a local already in scope), a
// Print, a bare discarded-expression statement (an ExpressionStatement), and —
// since the non-tail if/switch work — a conditional if statement (a tir.If
// built by buildLeadingIf: arms are fall-through sequences, optional else)
// and a switch statement (a tir.Switch built by buildLoopSwitch: case bodies
// may fall through or return). The If and Switch cases are how a leading
// function-body statement sequence admits ordinary, non-tail control flow: an
// `if` or `switch` followed by more statements.
// context names the enclosing construct in error messages; indent is the
// statement's C indentation; depth is the enclosing block's nesting depth,
// passed to the If/Switch builders so they open their arms/case bodies one
// level deeper. scope is the set of in-scope locals, each mapped
// to a localInfo recording the resolved type it was declared with: the entry's
// integer width or bool for a scalar local, the tuple type's TypeID for a
// tuple local, the array type's TypeID for an array local, the optional
// type's TypeID for an optional local, or the struct type's TypeID for a
// struct local. An Initialize adds its symbol to it once validated, a Store
// reads it. width is
// the entry's resolved integer width; an integer local's C type name
// follows it (int32_t for an i32 entry, int64_t for an i64 entry), and a
// local whose value carries the other width is rejected by buildExpr, so an
// i32 local inside an i64 entry (or vice versa) is a clean width-mismatch
// error, not an attempted coercion. A local whose value carries the bool
// builtin is a bool local, declared as C `bool` and built by buildBoolExpr;
// its scope entry records types.Bool so a later reference or reassignment is
// emitted and validated against the same type. A local whose value carries a
// struct type is a struct local: it is declared as C `pebble_struct_<typeID>_t`
// and initialized from a struct literal (see buildStructLocalDeclaration), and
// its scope entry records the struct type so a later field read resolves the
// struct type being projected (see buildExpr's Load case). A local whose value
// carries a
// tuple type is a tuple local: it is declared as C `pebble_tuple_<typeID>_t`,
// initialized from a tuple literal whose element expressions are each built by
// the grammar their own element type selects, and its scope entry records the
// tuple type so a later element read resolves the element being indexed (see
// buildExpr's Load case). The
// caller is responsible for having already cloned scope if the statements must
// not leak into a sibling or enclosing scope (buildBlock and buildLoopBody both
// do). Any other statement kind is a clean rejection naming what was found.
func buildLeadingStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("%s references invalid statement node %d", context, id)
	}
	switch statement.Kind {
	case tir.Initialize:
		if len(statement.Children) != 1 {
			return "", fmt.Errorf("%s local declaration initializes %d value(s), want exactly one expression", context, len(statement.Children))
		}
		if _, declared := scope[statement.Symbol]; declared {
			return "", fmt.Errorf("%s declares local %d more than once", context, statement.Symbol)
		}
		initValue, ok := unit.Node(statement.Children[0])
		if !ok {
			return "", fmt.Errorf("%s local declaration references invalid value node %d", context, statement.Children[0])
		}
		if isTuple(snapshot, initValue.Type) {
			// A tuple-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself — confirmed against
			// a real fixture). The supported initializer is a tuple literal
			// (TupleValue); every other tuple initializer shape is a clean
			// rejection.
			return buildTupleLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isArray(snapshot, initValue.Type) {
			if initValue.Kind == tir.DirectCall {
				return buildArrayCallInitializer(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			return buildArrayLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isOptional(snapshot, initValue.Type) {
			// An optional-typed local: its type is the initializer value's
			// Type (the Initialize node carries no Type itself, confirmed
			// against a real fixture — same as tuple/array locals). The
			// supported initializers are SomeOptional (some <expr>), none, and
			// since the OptionalIntegerToEnum slice an integer-to-optional-
			// enum cast (`5 as ?Color`); every other optional initializer
			// shape is a clean rejection.
			return buildOptionalLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, id)
		}
		if isEnumType(unit, snapshot, initValue.Type) {
			// An enum-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above), and the
			// type is Nominal exactly like a struct's (see isEnumType), so
			// this check must precede the struct check below. The type is a
			// tagged union (10.35) exactly when the caller's union map, built
			// from reachable payload-carrying constructions, contains it: such
			// a local is declared as the union's tagged struct and initialized
			// from a variant construction (see buildUnionLocalDeclaration). A
			// plain-enum-typed local is declared as the enum typedef and
			// initialized from a variant literal (an EnumVariantValue, e.g.
			// Color.green, or a zero-payload VariantConstruct, e.g.
			// Color.red()); every other enum initializer shape is a clean
			// rejection.
			if _, isUnion := unions[initValue.Type]; isUnion {
				return buildUnionLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, unions, width)
			}
			return buildEnumLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStruct(snapshot, initValue.Type) {
			if runtimeType(unit, snapshot, initValue.Type) != 0 {
				return buildRuntimeLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			// A struct-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as tuple/array/optional locals). The
			// supported initializer is a RecordConstruct (a struct literal);
			// every other struct initializer shape is a clean rejection.
			return buildStructLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStr(snapshot, initValue.Type) {
			// A str-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above). The
			// supported initializer is a StringLiteral (a string literal), or
			// since 10.36 a call to a str-returning helper (a DirectCall whose
			// result type is str, the one supported call position for
			// declaring a str local); every other str initializer shape is a
			// clean rejection.
			return buildStrLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isSlice(snapshot, initValue.Type) {
			// A slice-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializer is a CheckedSlice
			// (a slice expression like a[1:3]), or since 10.38 a call to a
			// slice-returning helper (a DirectCall whose result type is the
			// slice type, the one supported call position for declaring a
			// slice local); every other slice initializer
			// shape is a clean rejection.
			return buildSliceLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isPointer(snapshot, initValue.Type) {
			// A pointer-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializers are an AddressOf
			// expression (`let p *i32 = &y;`), another pointer-typed local
			// (pointer-to-pointer copy), a nil literal, a pointer-returning
			// call, or an explicit pointer-to-pointer cast; every other
			// pointer initializer shape is a clean rejection.
			return buildPointerLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isFunctionType(snapshot, initValue.Type) {
			// A function-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializer is a function value —
			// a bare top-level function reference (a HoistedFunctionValue) or
			// another function-typed local (a SymbolValue); every other
			// function initializer shape is a clean rejection (see
			// buildFunctionLocalDeclaration).
			return buildFunctionLocalDeclaration(unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		pre, core, err := buildScalarInitializeCore(unit, snapshot, fileSet, statement, initValue, scope, context, width)
		if err != nil {
			return "", err
		}
		// A local that a later statement never reads would otherwise trigger
		// -Wunused-variable under the mandated -Wall -Wextra -Werror; a
		// redundant (void) cast is a no-op when the local IS read later, so it
		// is emitted unconditionally rather than tracking whether a use
		// actually follows. buildScalarInitializeCore produced the declaration
		// core (`<cType> pebble_local_<id> = <expr>`); the indent, the
		// statement-terminating `;`, and the (void) cast turn it into this
		// full statement, byte-identical to before this helper was extracted.
		// A non-empty pre (a temp declaration for a force-unwrap of a call
		// result, e.g. `let v = m.get(5)!;`) is threaded as an extra leading
		// statement, the same mechanical shape buildForInitClause returns its
		// OptionalIntegerToEnum pre.
		if pre != "" {
			return indent + pre + "\n" + indent + core + ";\n" + indent + fmt.Sprintf("(void)pebble_local_%d;", statement.Symbol), nil
		}
		return indent + core + ";\n" + indent + fmt.Sprintf("(void)pebble_local_%d;", statement.Symbol), nil
	case tir.Store:
		// A Store reassigns a local declared earlier in this block or an
		// enclosing one; it does not declare a new symbol, so it never
		// touches scope. The checker refuses to emit a Store targeting a
		// `let` (C0606: the assignment place is not writable), so any
		// Store this backend sees, from real source, necessarily targets
		// a `var`. The value text is built by buildStoreCore (shared with the
		// for-loop update clause); the indent and the trailing `;` turn it
		// into this full statement, byte-identical to before the helper was
		// extracted.
		core, err := buildStoreCore(unit, snapshot, fileSet, statement, scope, context, width, unions)
		if err != nil {
			return "", err
		}
		return indent + core + ";", nil
	case tir.CompoundStore:
		// A CompoundStore is a compound assignment — `i += 1;`, `arr[j] -= 1;`,
		// `self.field *= 2;`, `*p /= 3;`, `x %= 4;` — or a postfix `i++` /
		// `i--` (the checker builds a postfix update as a CompoundStore with +
		// or - and a literal-one value child, so one emission covers both). It
		// does not declare a new symbol, so it never touches scope, exactly
		// like a Store. The value text is built by buildCompoundStore (shared
		// with the for-loop update clause); the indent and the trailing `;`
		// turn it into this full statement, mirroring the Store case.
		pre, core, err := buildCompoundStore(unit, snapshot, fileSet, id, statement, scope, context, width)
		if err != nil {
			return "", err
		}
		if pre != "" {
			return indent + pre + "\n" + indent + core + ";", nil
		}
		return indent + core + ";", nil
	case tir.ExpressionStatement:
		// A bare discarded-expression statement — `helper();` written as its
		// own statement, its result unused (a tir.ExpressionStatement wrapping
		// exactly one value, built by the checker's controlExpression case).
		// The only supported shape is a call to a void-returning function,
		// emitted as a bare C statement by the shared buildExpressionStatement
		// (also used by buildDeferredStatements for a deferred call); any other
		// discarded expression — a non-call value, or a call to a non-void-
		// returning function — is a clean rejection naming what was found.
		return buildExpressionStatement(unit, snapshot, fileSet, statement, scope, indent, context, width)
	case tir.Print:
		// A print statement — `print a, b, c;` — emitted as one combined
		// printf call by the shared buildPrint (also used by buildLoopBody's
		// explicit Print case and buildDeferredStatements for a deferred
		// print), so the emission logic lives in exactly one place. A
		// slice-index operand's temp declaration is returned as a leading
		// pre-statement and threaded into this statement sequence before the
		// printf line, exactly as a return threads its pre-return temp.
		pre, line, err := buildPrint(unit, snapshot, fileSet, statement, scope, indent, context, width)
		if err != nil {
			return "", err
		}
		if pre != "" {
			return pre + "\n" + line, nil
		}
		return line, nil
	case tir.If:
		// A conditional if statement as an ordinary leading statement — the
		// non-tail shape, e.g. a guard clause `if x > 0 { return 1; }`
		// followed by more code. Its arms are fall-through statement sequences
		// (no required tail, optional else), built by buildLeadingIf, the
		// top-level twin of buildLoopIf.
		return buildLeadingIf(unit, snapshot, fileSet, statement, scope, depth, width, result, unions, context)
	case tir.Switch:
		// A switch statement as an ordinary leading statement — a non-tail
		// switch whose case bodies may fall through or return, built by
		// buildLoopSwitch (the same fall-through switch the loop-body/arm
		// position uses). This is the only place a top-level function body can
		// contain a non-tail switch; see buildLoopSwitch.
		return buildLoopSwitch(unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	default:
		return "", fmt.Errorf("%s statement is a %s, want a local declaration (Initialize), a reassignment (Store), a compound assignment or postfix increment/decrement (CompoundStore), a call to a void-returning function used as a statement (ExpressionStatement), a print statement (Print), a conditional if statement (If), or a switch statement (Switch)", context, statement.Kind)
	}
}

// buildExpressionStatement builds the C statement text for one
// tir.ExpressionStatement — a bare discarded-expression statement such as
// `helper();` written as its own statement, produced by the checker's
// controlExpression case with no StatementForm set and a single value child
// (confirmed against real fixtures). It is the statement-context twin of a
// value-context call: a tir.DirectCall to a function of ANY result type — a
// void-returning helper called purely for its side effect, or a call to a
// non-void-returning function whose result is deliberately discarded (legal
// Pebble, and ordinary C: discarding a function's return value at a bare call
// statement is always allowed, never warning even under -Wall -Wextra -Werror)
// — emitted as `pebble_fn_<calleeSymbolID>(ctx, <args>);` at the given indent,
// mirroring buildDirectCall's two return shapes exactly
// (`pebble_fn_<sym>(ctx)` with no arguments, `pebble_fn_<sym>(ctx, <args>)`
// with some) but as a statement instead of a value expression. The callee is
// resolved through findFunctionDeclaration, and its result type places no
// restriction on the shape: the call is emitted identically whether it returns
// void or a value, and whatever it returns is discarded by C. The call text
// itself is built by buildDirectCall unchanged, so argument building, context
// threading, and the convention/context-action checks are identical to a
// value-context call. Any other ExpressionStatement child — a discarded
// non-call expression (anything that is not a DirectCall, MethodCall, or
// IndirectCall — the latter handled separately above) — is a clean rejection
// naming what was found. The function is shared by buildLeadingStatement's
// ExpressionStatement case (which covers both buildBlock's and buildLoopBody's
// leading-statement sequences) and buildDeferredStatements' deferred-statement
// case, so the emission logic lives in exactly one place.
func buildExpressionStatement(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if len(statement.Children) != 1 {
		return "", fmt.Errorf("%s discarded-expression statement has %d child(ren), want exactly one (the expression being discarded)", context, len(statement.Children))
	}
	expr, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", fmt.Errorf("%s discarded-expression statement references invalid value node %d", context, statement.Children[0])
	}
	if expr.Kind == tir.IndirectCall {
		callExpr, err := buildIndirectCall(unit, snapshot, fileSet, expr, scope, width)
		if err != nil {
			return "", err
		}
		return indent + callExpr + ";", nil
	}
	if expr.Kind != tir.DirectCall && expr.Kind != tir.MethodCall {
		return "", fmt.Errorf("%s discarded-expression statement discards a %s, which is not supported as a bare statement yet (only a direct, method, or indirect call is)", context, expr.Kind)
	}
	callPre, callExpr, err := buildDirectCallWithPre(unit, snapshot, fileSet, expr, scope, width)
	if err != nil {
		return "", err
	}
	// A bare call statement is a leading-statement position, so an inline
	// slice-construction argument's temp declaration is emitted as a preceding
	// statement line at the same indent (the buildScalarInitializeCore pre
	// threading shape).
	if callPre != "" {
		return indent + callPre + "\n" + indent + callExpr + ";", nil
	}
	return indent + callExpr + ";", nil
}

// buildPrint builds the C text for one print statement — a tir.Print whose
// Children are the printed operands in source order, one node per operand
// (built by the checker's controlPrint case from `print a, b, c;`, each
// operand independently type-checked). The emission matches v1's print
// codegen shape exactly: ONE combined printf call per print statement, not
// one per operand — every operand's format specifier is concatenated into a
// single format string (ending in the literal `\n`, so every print statement
// produces exactly one line of output) and every operand's value is a single
// argument, in the same order. The checker already restricts print operands to
// exactly bool, char, str, any integer builtin, or any float builtin (C0612 —
// a nominal enum operand like `print Color.red;` is rejected upstream), so
// this dispatch is exactly the set of values this backend already knows how
// to build, each through its OWN existing builder, never a new value-building
// path:
//
//   - integer (Int/Uint/I8/I16/I32/I64/U8/U16/U32/U64) — buildExpr at the
//     operand's own resolved integer kind, with the format specifier chosen
//     from the <inttypes.h> PRId8/PRId16/PRId32/PRId64/PRIu8/PRIu16/PRIu32/
//     PRIu64 macros (string-literal-concatenated into the format string at
//     compile time) so the specifier's width exactly matches the fixed-width
//     C type cType produces for the operand — never a hand-picked %hhd/%hd
//     whose width only happens to match — keeping the mandated
//     -Wall -Wextra -Werror build -Wformat-clean for every integer width.
//   - bool — buildBoolExpr, wrapped in a C ternary (`<expr> ? "true" :
//     "false"`) so the printed text is the word true/false; the format
//     specifier is %s, exactly v1's approach of making the bool argument a
//     const char * before the format string is built.
//   - char — buildCharOperand (a char's C type is the fixed int32_t scalar
//     value, which this backend never hands to printf directly): the scalar is
//     encoded to UTF-8 by the runtime helper pebble_rt_char_to_utf8 into a
//     fresh per-operand uint8_t[5] buffer declared in the print's leading
//     pre-statements, and the combined printf gets the buffer cast to
//     const char * under a %s specifier. A bare %c would write only one byte,
//     corrupting every char beyond U+007F, so every char operand — ASCII or
//     not — goes through the helper.
//   - str — buildStrOperand (a str local, a string literal, or a call to a
//     str-returning helper), with the argument being the value's .data field
//     cast to const char * (the runtime's PebbleStr is
//     { const uint8_t *data; size_t len; }, and %s wants a char *; the
//     explicit cast is what keeps the emitted call -Wformat-clean), specifier
//     %s.
//   - float (F32/F64) — buildFloatExpr at the operand's own float kind;
//     f32/f64 promote to double in a variadic call either way, so %f for both
//     is correct and matches v1.
//
// The emitted statement is:
//
//	<indent>printf("<spec0><spec1>...\n", <arg0>, <arg1>);
//
// with the \n spelled as the C two-character escape, and no arguments when the
// print has no operands (only reachable from hand-built IR — the parser
// requires at least one expression — emitting printf("\n") to print a blank
// line, matching v1's zero-expression print). Each integer specifier is
// emitted as the macro name OUTSIDE the surrounding quotes (`"%"PRId32`, not
// `"%PRId32"`), so the preprocessor expands the macro to the exact-width
// specifier text and adjacent-literal concatenation folds it into the format
// string; the bool/char/str/float specifiers are the plain `%s`/`%s`/`%s`/`%f`
// literals (a char's %s is backed by the pre-statement UTF-8 buffer above).
// Every operand value is built
// under the grammar its own resolved type selects; a print operand of any type
// the checker does not allow as printable is a clean rejection naming what was
// found, never guessed. The function is shared by buildLeadingStatement's
// Print case (which covers both buildBlock's and buildLoopBody's
// leading-statement sequences), buildLoopBody's explicit Print case, and
// buildDeferredStatements' deferred-statement case, so the emission logic
// lives in exactly one place.
func buildPrint(unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	var formatParts []string
	var args []string
	var preParts []string
	for _, childID := range statement.Children {
		child, ok := unit.Node(childID)
		if !ok {
			return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, childID)
		}
		// A parenthesized operand — `print ("hi")` — arrives wrapped in
		// tir.SourceAlias nodes (one per grouping level, confirmed against a
		// real fixture dump), which record grouped-expression parens and
		// nothing else, so the operand is unwrapped to its innermost node
		// before its type is dispatched on. Unwrapping here keeps the
		// per-type value builders untouched (buildExpr/buildBoolExpr/
		// buildFloatExpr unwrap a SourceAlias themselves, but buildCharOperand
		// and buildStrOperand have no SourceAlias case); the unwrapped node
		// carries the same Type the SourceAlias did, so the dispatch below is
		// exactly what the checker validated.
		operandID := childID
		for child.Kind == tir.SourceAlias {
			if len(child.Children) != 1 {
				return "", "", fmt.Errorf("%s print operand is a SourceAlias with %d child(ren), want exactly one", context, len(child.Children))
			}
			operandID = child.Children[0]
			child, ok = unit.Node(operandID)
			if !ok {
				return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, operandID)
			}
		}
		if child.Kind == tir.InterpolatedString {
			for _, part := range child.Parts {
				switch part.Kind {
				case tir.InterpolationTextPart:
					formatParts = append(formatParts, `"%s"`)
					args = append(args, `(const char *)"`+escapeCString(part.Text)+`"`)
				case tir.InterpolationValuePart:
					valueNode, ok := unit.Node(part.Value)
					if !ok {
						return "", "", fmt.Errorf("%s interpolated-string print operand references invalid value node %d", context, part.Value)
					}
					valueKind, ok := resolvedBuiltin(snapshot, valueNode.Type)
					if !ok || valueKind != types.Bool {
						return "", "", fmt.Errorf("%s interpolated-string print operand interpolates a %s of type %s, want bool", context, valueNode.Kind, describeType(snapshot, valueNode.Type))
					}
					formatParts = append(formatParts, `"%s"`)
					boolExpr, err := buildBoolExpr(unit, snapshot, fileSet, part.Value, scope, width)
					if err != nil {
						return "", "", err
					}
					args = append(args, "("+boolExpr+` ? "true" : "false")`)
				default:
					return "", "", fmt.Errorf("%s interpolated-string print operand has an unknown part kind %d", context, part.Kind)
				}
			}
			continue
		}
		kind, ok := resolvedBuiltin(snapshot, child.Type)
		if !ok {
			return "", "", fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
		}
		// A bare CheckedIndex whose base is a SLICE-typed value (not a str) —
		// `print view()[0];`, indexing a call's slice result directly, the
		// confirmed real-world gap this slice closes. The indexed element read
		// needs the base materialized into a temp local (see
		// buildSliceIndexValue), whose temp-declaration statement this print
		// position — a statement sequence — hosts as a leading pre-statement,
		// exactly as buildReturnStatement threads a pre-return temp. A
		// str-index base (char result) stays on buildCharOperand's own
		// CheckedIndex case below.
		isSliceIndex := false
		if child.Kind == tir.CheckedIndex {
			strBase, err := checkedIndexBaseIsStr(unit, snapshot, child)
			if err != nil {
				return "", "", err
			}
			isSliceIndex = !strBase
		}
		var arg string
		var err error
		var sliceIndexPre string
		var charPreParts []string
		switch {
		case cType(kind) != "":
			// An integer operand of any builtin width, not just the entry's
			// own: its value is built by buildExpr at its own resolved kind
			// (re-checking every node in the expression carries that width,
			// exactly as a scalar local declaration does), and its specifier
			// comes from the <inttypes.h> PRI* macros whose expansion matches
			// the operand's fixed-width C type. The macro name is emitted
			// OUTSIDE the string quotes (`"%"PRId32`), so the preprocessor
			// expands it to the specifier text and the adjacent literals
			// concatenate into the format string — never spelled
			// `"%PRId32"`, which would put a literal invalid `%P` specifier
			// in the format. A slice-index operand's read (element C type)
			// carries the same width as its element, so the same specifier
			// applies.
			formatParts = append(formatParts, `"%"`+printfSpecifier(kind))
			if isSliceIndex {
				sliceIndexPre, arg, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, false)
			} else {
				arg, err = buildExpr(unit, snapshot, fileSet, operandID, scope, kind, width)
			}
		case kind == types.Bool:
			// A bool operand prints as the words true/false: build the bool
			// expression under the bool grammar, then wrap it in the C ternary
			// that selects the const char * literal, so the %s specifier's
			// argument is already the pointer the format string wants — v1's
			// own approach for bool in print. A slice-index operand's read is
			// a C bool, so the same ternary wraps it.
			formatParts = append(formatParts, `"%s"`)
			if isSliceIndex {
				var read string
				sliceIndexPre, read, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, true)
				if err == nil {
					arg = "(" + read + " ? \"true\" : \"false\")"
				}
			} else {
				arg, err = buildBoolExpr(unit, snapshot, fileSet, operandID, scope, width)
				if err == nil {
					arg = "(" + arg + " ? \"true\" : \"false\")"
				}
			}
		case kind == types.Char:
			// A char operand prints as the UTF-8 encoding of the single
			// character its int32_t scalar value encodes. The scalar is built
			// under the char grammar (buildCharOperand, or the slice-index
			// read for a slice-element operand — both yield the same int32_t
			// char value), then the runtime helper pebble_rt_char_to_utf8
			// encodes it — 1-4 UTF-8 bytes plus the trailing NUL — into a
			// fresh per-operand uint8_t[5] buffer declared in this print's
			// pre-statements, and the combined printf's %s consumes that
			// buffer. The scalar is never passed to printf directly: a %c
			// writes only a single byte, so any char beyond U+007F would print
			// corrupt bytes instead of its full UTF-8 sequence (the exact bug
			// this encoding closes); routing every char operand — ASCII
			// included — through the helper keeps the emitted C uniform. The
			// buffer name derives from the operand's own unwrapped node ID —
			// the stable identity of this operand, unique across the unit — so
			// a print with several char operands gets one distinct buffer each
			// with no collision, the same pebble_slice_index_<nodeID>
			// convention buildSliceIndexValue uses for its temp.
			formatParts = append(formatParts, `"%s"`)
			var scalar string
			if isSliceIndex {
				sliceIndexPre, scalar, err = buildSliceIndexValue(unit, snapshot, fileSet, operandID, child, scope, width, false)
			} else {
				scalar, err = buildCharOperand(unit, snapshot, fileSet, operandID, scope, width)
			}
			if err == nil {
				bufferName := fmt.Sprintf("pebble_char_utf8_%d", operandID)
				charPreParts = append(charPreParts,
					fmt.Sprintf("uint8_t %s[5];", bufferName),
					fmt.Sprintf("pebble_rt_char_to_utf8(%s, %s);", scalar, bufferName))
				arg = "(const char *)" + bufferName
			}
		case kind == types.Str:
			// A str operand prints its bytes: the value is built under the
			// str grammar, and the %s argument is the value's .data field cast
			// to const char * (the reachable str values this backend builds
			// all originate from NUL-terminated C string literals, so %s
			// reads exactly the intended bytes).
			formatParts = append(formatParts, `"%s"`)
			arg, err = buildStrOperand(unit, snapshot, fileSet, operandID, scope, width)
			if err == nil {
				arg = "(const char *)" + arg + ".data"
			}
		case kind == types.F32 || kind == types.F64:
			// A float operand prints with %f; f32/f64 promote to double in a
			// variadic call either way, so the one specifier covers both,
			// matching v1. The value is built under the float grammar at its
			// own float kind.
			formatParts = append(formatParts, `"%f"`)
			arg, err = buildFloatExpr(unit, snapshot, fileSet, operandID, scope, kind)
		default:
			return "", "", fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
		}
		if err != nil {
			return "", "", err
		}
		if sliceIndexPre != "" {
			preParts = append(preParts, indent+sliceIndexPre)
		}
		for _, pre := range charPreParts {
			preParts = append(preParts, indent+pre)
		}
		args = append(args, arg)
	}
	line := indent + "printf(" + strings.Join(formatParts, "") + `"\n"`
	if len(args) != 0 {
		line += ", " + strings.Join(args, ", ")
	}
	return strings.Join(preParts, "\n"), line + ");", nil
}
