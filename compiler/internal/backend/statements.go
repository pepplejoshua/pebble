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
func buildBlock(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, blockID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
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
			whileText, err := buildWhile(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
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
			rangeText, err := buildRangeLoop(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
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
			forText, err := buildFor(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
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
		text, err := buildLeadingStatement(st, unit, snapshot, fileSet, block.Children[i], scope, indent, depth, "entry function body block", width, result, unions)
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
		text, err := buildReturnStatement(st, unit, snapshot, fileSet, last, scope, indent, "entry function body block", width, result, unions)
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
		deferText, err := buildDeferredStatements(st, unit, snapshot, fileSet, last.DeferChain, scope, indent, "entry function body block", width, result, unions)
		if err != nil {
			return "", err
		}
		if deferText != "" {
			statements = append(statements, deferText)
		}
	case tir.If:
		ifText, err := buildIf(st, unit, snapshot, fileSet, last, scope, depth, width, result, unions)
		if err != nil {
			return "", err
		}
		statements = append(statements, ifText)
	case tir.Switch:
		switchText, err := buildSwitch(st, unit, snapshot, fileSet, last, scope, depth, width, result, unions)
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
			whileText, err := buildWhile(st, unit, snapshot, fileSet, last, scope, depth, width, result, unions)
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
func buildReturnStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, returnNode tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(returnNode.Children) != 1 {
		if len(returnNode.Children) == 0 && result.kind == types.Void {
			// A bare `return;` inside a void-returning helper's body — the
			// std/hmap.peb maybe_grow shape (`if self.cap == 0 { self.rehash(8);
			// return; }`): a return with no value is only legal in a void
			// function, and lowers to a plain C `return;` after any deferred
			// statements fire, exactly as the void helper's ImplicitReturn tail
			// emits nothing but its deferred statements.
			deferText, err := buildDeferredStatements(st, unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, result, unions)
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
		returnValue, err = buildBoolExpr(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isChar {
		// The enclosing function returns char (a reachable helper whose
		// ResultType is char — the entry always threads a scalar resultInfo),
		// so the return value is built under the char grammar by
		// buildCharOperand rather than buildExpr, which rejects a
		// char-typed value. Supported return shapes are a SymbolValue
		// naming a char-typed local in scope, a char literal, or a call to
		// another char-returning helper.
		returnValue, err = buildCharOperand(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.isStr {
		// The enclosing function returns str (a reachable helper whose
		// ResultType is str — the entry always threads a scalar resultInfo),
		// so the return value is built under the str grammar by
		// buildStrOperand rather than buildExpr, which rejects a str-typed
		// value. Supported return shapes are a SymbolValue naming a
		// str-typed local in scope, a string literal, or a call to another
		// str-returning helper.
		returnValue, err = buildStrOperand(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width)
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
		preReturn, returnValue, err = buildAggregateReturnValue(st, unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
	} else if result.unionType != 0 {
		// The enclosing function returns a tagged union (a reachable helper
		// whose ResultType is a tagged-union type — the entry always threads
		// a scalar resultInfo), so the return value is built under the union
		// grammar by buildUnionValueExpr rather than buildAggregateReturnValue
		// (which would reject an EnumVariantValue/VariantConstruct as a struct
		// return) or buildExpr. Supported return shapes are a SymbolValue
		// naming a union-typed local or parameter in scope of the matching
		// type, a fresh variant construction (an EnumVariantValue /
		// VariantConstruct with a payload), a union-typed struct field read, or
		// a union-payload optional force-unwrap; anything else is a clean
		// rejection.
		returnValue, err = buildUnionValueExpr(st, unit, snapshot, fileSet, returnNode.Children[0], scope, "entry function body return statement", result.unionType, width)
	} else if result.enumType != 0 {
		// The enclosing function returns a plain enum (a reachable helper
		// whose ResultType is an enum type — the entry always threads a scalar
		// resultInfo), so the return value is built under the enum grammar by
		// buildEnumValue rather than buildAggregateReturnValue (which would
		// reject an EnumVariantValue as a struct return) or buildExpr.
		// Supported return shapes are a variant literal (an
		// EnumVariantValue / payload-less VariantConstruct), a SymbolValue
		// naming an enum-typed local or parameter in scope, an integer-to-enum
		// cast, an enum-typed struct field read, or an enum-payload optional
		// force-unwrap; anything else is a clean rejection.
		returnValue, err = buildEnumValue(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else if result.arrayType != 0 {
		// The enclosing function returns an array (a reachable helper whose
		// ResultType is an array type), so the return value is built under the
		// array grammar by buildArrayReturnValue rather than buildExpr, which
		// rejects an array-typed value. Supported return shapes are a
		// SymbolValue naming an array-typed local in scope, an ArrayValue
		// literal, or an ArrayRepeat. An ArrayRepeat (a single [v; N] source
		// expression evaluated exactly once) needs a C temp to hold the value so
		// it is not re-evaluated once per slot; a return is a pure expression
		// position with nowhere to place the temp declaration, so, exactly like
		// the slice branch below, the temp-declaration statement is threaded
		// into the statement sequence as an extra pre-return statement.
		preReturn, returnValue, err = buildArrayReturnValue(st, unit, snapshot, fileSet, returnNode.Children[0], scope, result.arrayType, indent, width)
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
		preReturn, returnValue, err = buildSliceReturnValue(st, unit, snapshot, fileSet, returnNode.Children[0], scope, result, indent, width)
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
		returnValue, err = buildOptionalValue(st, unit, snapshot, fileSet, returnNode.Children[0], scope, result.optionalType, "entry function body return statement", width)
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
		returnValue, err = buildFunctionValue(st, unit, snapshot, fileSet, mustNode(unit, returnNode.Children[0]), scope, "entry function body return statement", width)
	} else if result.kind == types.F32 || result.kind == types.F64 {
		// A float-returning entry (a main declared to return f32/f64 — the
		// one float-returning position Float Stage A supports; float helper
		// results are rejected upstream by validateHelperSignature, so only
		// the entry's resultInfo can carry a float kind), so the return
		// value is built under the float grammar by buildFloatExpr rather
		// than buildExpr, which rejects a float-typed value. Supported
		// return shapes are a float literal or a SymbolValue naming a
		// float-typed local in scope of the same float kind.
		returnValue, err = buildFloatExpr(st, unit, snapshot, fileSet, returnNode.Children[0], scope, result.kind, width)
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
		returnValue, err = buildUintExpr(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width)
	} else {
		returnValue, err = buildExpr(st, unit, snapshot, fileSet, returnNode.Children[0], scope, width, width)
	}
	if err != nil {
		return "", err
	}
	deferText, err := buildDeferredStatements(st, unit, snapshot, fileSet, returnNode.DeferChain, scope, indent, context, width, result, unions)
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
func buildSwitch(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(st, unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, false)
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
func buildLoopSwitch(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildSwitchStatement(st, unit, snapshot, fileSet, switchNode, locals, depth, width, result, unions, true)
}

// buildSwitchStatement is the shared core behind buildSwitch and
// buildLoopSwitch: it validates and builds the C text for a switch statement
// with exactly the same subject-building, case-grouping, and label emission in
// both positions. The only difference is the case-body builder selected by
// fallthrough: false selects buildSwitchCaseBody (each body must end in a
// return), true selects buildLoopSwitchCaseBody (each body is an ordinary
// fall-through statement sequence that may or may not return).
func buildSwitchStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if len(switchNode.Children) < 2 {
		return "", fmt.Errorf("switch statement has %d child(ren), want at least 2 (the subject and one case)", len(switchNode.Children))
	}
	// Build the subject expression. The subject's resolved type decides the
	// grammar: an integer subject of any fixed-width integer builtin (the
	// entry's width, the abstract int, or a narrower/wider fixed-width
	// integer like u8 or i16) is built by buildExpr at the subject's own
	// width, a uint subject by buildUintExpr (uint is the word-sized
	// unsigned builtin, a DISTINCT builtin from u64, so it needs its own
	// grammar rather than the fixed-width branch), a bool subject by
	// buildBoolExpr, a char subject by
	// buildCharOperand, a tagged-union subject by
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
	// subjectIntWidth is the subject's own resolved integer builtin: a
	// fixed-width integer (i8/i16/i32/i64 or u8/u16/u32/u64 — the entry's
	// width being just one of them) or uint (whose case labels are spelled at
	// uint's own width), or zero when the subject is not a concrete
	// fixed-width/word-sized integer (an abstract-int, bool, char, str, or
	// enum/tagged-
	// union subject). buildCaseLabel spells integer case labels at THIS
	// width rather than the ambient entry width, so the C switch compares the
	// subject's own C type against matching-width constants (a u8 subject
	// gets `case 5u:`, never a silent truncation or a sign mismatch).
	var subjectIntWidth types.BuiltinKind
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
					if ginfo, isGlobal := st.globals[subjectNode.Symbol]; isGlobal {
						if ginfo.info.enumType != enumSubject {
							return "", fmt.Errorf("switch subject references global symbol %d, a global of type %s, not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, ginfo.info.enumType), unionTypeName(enumSubject))
						}
						subjectExpr = fmt.Sprintf("pebble_global_%d.tag", subjectNode.Symbol)
						break
					}
					if einfo, isExtern := st.externData[subjectNode.Symbol]; isExtern {
						if einfo.info.enumType != enumSubject {
							return "", fmt.Errorf("switch subject references extern variable symbol %d, an extern variable of type %s, not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, einfo.info.enumType), unionTypeName(enumSubject))
						}
						subjectExpr = einfo.name + ".tag"
						break
					}
					return "", fmt.Errorf("switch subject references symbol %d, which is not an enum-typed local declared earlier in the body", subjectNode.Symbol)
				}
				if info.enumType != enumSubject {
					return "", fmt.Errorf("switch subject references symbol %d, a local of type %s, not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, info.enumType), unionTypeName(enumSubject))
				}
				subjectExpr = fmt.Sprintf("pebble_local_%d.tag", subjectNode.Symbol)
			case tir.DirectCall:
				// A call to a union-returning helper used directly as the
				// switch subject (`switch make_result() { ... }`, confirmed
				// checker-reachable). Mirroring buildUnionValueExpr's
				// DirectCall case, the call's own Type (the callee's resolved
				// result type) is double-checked to be exactly the subject's
				// union type, and the call is built by buildDirectCallNested —
				// the pure-expression-position call machinery, so an inline
				// slice-construction argument folds correctly into a call
				// expression the switch statement can place directly as its
				// subject. The whole call expression is directly a union
				// value of exactly enumSubject, so `.tag` reads the stored
				// discriminant the same way the SymbolValue and
				// VariantConstruct branches do. The call appears exactly
				// once in the emitted C — the C switch evaluates its
				// controlling expression a single time at dispatch, and the
				// case bodies never re-read the subject.
				if subjectNode.Type != enumSubject {
					return "", fmt.Errorf("switch subject is a call to symbol %d whose declared result type %s is not the subject's union type %s", subjectNode.Symbol, describeType(snapshot, subjectNode.Type), unionTypeName(enumSubject))
				}
				callExpr, callErr := buildDirectCallNested(st, unit, snapshot, fileSet, subjectNode, locals, width)
				if callErr != nil {
					return "", callErr
				}
				subjectExpr = callExpr + ".tag"
			case tir.VariantConstruct, tir.EnumVariantValue:
				construction, buildErr := buildUnionConstruction(st, unit, snapshot, fileSet, subjectNode, locals, "switch subject", unions[enumSubject], width)
				if buildErr != nil {
					return "", buildErr
				}
				subjectExpr = construction + ".tag"
			case tir.Load, tir.FieldValue, tir.CheckedOptionalUnwrap, tir.SourceAlias:
				// A tagged-union value read from any other source, used
				// directly as the switch subject: a union-typed struct field
				// read (`switch s.u { ... }`, a Load of a FieldPlace), a whole
				// union read through a pointer deref (`switch *p { ... }`, a
				// Load of a DereferencePlace), a union field of a non-
				// addressable struct value (`switch mk().u { ... }`, a
				// FieldValue), a force-unwrap of a union-payload optional
				// (`switch o! { ... }`, a CheckedOptionalUnwrap), or a
				// parenthesized union value (`switch (c) { ... }`, a
				// SourceAlias, transparently unwrapped to its child). These are
				// all lowered by the shared buildUnionValueExpr — the same
				// builder a union-typed call argument, optional payload, and
				// return forward use — to a value of the union's own C type,
				// and the switch compares the stored discriminant, so the
				// subject reads `.tag` exactly as the SymbolValue and
				// VariantConstruct branches do. The value is parenthesized so
				// the `.tag` projection applies to the WHOLE union expression
				// (a force-unwrap's ternary and a deref's cast would otherwise
				// let the postfix `.tag` bind to only their last operand).
				unionValue, buildErr := buildUnionValueExpr(st, unit, snapshot, fileSet, switchNode.Children[0], locals, "switch subject", enumSubject, width)
				if buildErr != nil {
					return "", buildErr
				}
				subjectExpr = fmt.Sprintf("(%s).tag", unionValue)
			default:
				return "", fmt.Errorf("switch subject is a %s of tagged-union type %s, want a reference to a union-typed local in scope or a union variant construction", subjectNode.Kind, unionTypeName(enumSubject))
			}
		} else {
			// A plain-enum-typed subject: a reference to an enum-typed local
			// (a SymbolValue) or a variant literal (an EnumVariantValue /
			// zero-payload VariantConstruct) — buildEnumValue handles all three.
			subjectExpr, err = buildEnumValue(st, unit, snapshot, fileSet, switchNode.Children[0], locals, width)
		}
	} else if integerSubjectWidth, integerSubject := resolvedBuiltin(snapshot, subjectNode.Type); integerSubject && cType(integerSubjectWidth) != "" && !isUint(snapshot, subjectNode.Type) && !isAbstractInt(snapshot, subjectNode.Type) {
		// Any concrete fixed-width integer subject, not just the entry's own
		// width: a u8, i16, u32, ... subject is built by buildExpr at the
		// subject's OWN resolved width (buildExpr's width gate admits a node
		// of any compatible fixed-width integer type at that width), so the C
		// switch compares the subject's own C type — a uint8_t local, for
		// example — against case labels that buildCaseLabel emits at the same
		// width. This is the same per-operand width resolution
		// buildComparisonOperand performs for comparison operands, and the
		// generic resolvedBuiltin/cType widening already applied to
		// struct-field reads (places.go), optional payloads, slice elements,
		// and the struct-field typedef itself. The ambient entry width is
		// threaded through as buildExpr's entryWidth parameter so any
		// width-requiring child (a checked runtime call) still knows the true
		// entry width. The abstract `int` builtin is deliberately excluded:
		// an unanchored-int subject is either an integer literal or a
		// SymbolValue, both handled directly by the two branches below.
		subjectIntWidth = integerSubjectWidth
		subjectExpr, err = buildExpr(st, unit, snapshot, fileSet, switchNode.Children[0], locals, integerSubjectWidth, width)
	} else if isUint(snapshot, subjectNode.Type) {
		// A uint-typed subject (the word-sized unsigned builtin,
		// snapshot.Builtins().Uint, deliberately excluded from the fixed-width
		// branch above): built by buildUintExpr, the same builder every other
		// uint value position in this backend uses (uint-typed parameters,
		// locals, returns, checked arithmetic, and range-loop bounds). uint's
		// C type is the fixed uint64_t (cType(types.Uint)), so subjectIntWidth
		// is set to types.Uint and buildCaseLabel spells the case labels at
		// that width — `case 5u:`, the "u" suffix integerLiteralText gives
		// every unsigned value — keeping the case constants' C type aligned
		// with the uint64_t subject.
		subjectIntWidth = types.Uint
		subjectExpr, err = buildUintExpr(st, unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	} else if isBool(snapshot, subjectNode.Type) {
		// A bool-typed subject: built by buildBoolExpr, the same builder every
		// other bool value position in this backend uses. The subject is cast
		// to int32_t for JUST the C switch (...) header: C's -Wswitch-bool
		// (enabled under the mandated -Werror) rejects a switch whose
		// controlling expression is itself a C bool, and bool's underlying
		// value is already 0 or 1, so the int32_t cast is the minimal C idiom
		// that satisfies the strict flags without changing the case labels
		// (buildCaseLabel spells bool case constants as `case 1:`/`case 0:`,
		// which an int32_t switch still compares correctly against). The
		// int32_t cast matches this backend's cast convention everywhere else
		// (char literals, slice-length arguments to checked-index calls).
		boolExpr, boolErr := buildBoolExpr(st, unit, snapshot, fileSet, switchNode.Children[0], locals, width)
		if boolErr != nil {
			return "", boolErr
		}
		subjectExpr = "(int32_t)" + boolExpr
	} else if isChar(snapshot, subjectNode.Type) {
		// A char-typed subject: built by buildCharOperand, the same builder
		// every other char-typed position in this backend uses. A char's C
		// type is the fixed int32_t, so the subject is an integral value the
		// C switch can compare against char-literal case labels (emitted by
		// buildCaseLabel as `case (int32_t)<scalar>:`).
		subjectExpr, err = buildCharOperand(st, unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	} else if isStr(snapshot, subjectNode.Type) {
		// A str-typed subject cannot use a native C switch (C switch labels
		// must be integer-constant expressions). Lowered as an if/else chain
		// calling the runtime helper pebble_rt_str_eq for each case.
		return buildStrSwitchStatement(st, unit, snapshot, fileSet, switchNode, subjectNode, locals, depth, width, result, unions, fallThrough)
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
		if name, ok := localOrGlobalName(st, subjectNode.Symbol, locals); ok {
			subjectExpr = name
		} else {
			return "", fmt.Errorf("switch subject references symbol %d, which is not a local in scope", subjectNode.Symbol)
		}
	} else {
		return "", fmt.Errorf("switch subject has type %s, want %s, bool, or char, or an enum/tagged-union type", describeType(snapshot, subjectNode.Type), wantName(width))
	}
	if err != nil {
		return "", err
	}
	// Validate enum-variant cases before grouping: every CaseValue must name
	// a variant of the subject enum (confirmed checker-reachable for an
	// enum/tagged-union subject). Scalar (non-CaseValue) cases are validated
	// by grouping and by case-label emission.
	if enumSubject != 0 {
		for _, caseID := range switchNode.Children[1:] {
			caseNode, ok := unit.Node(caseID)
			if !ok || caseNode.Kind != tir.SwitchCase || caseNode.HasElse || caseNode.CaseValue == 0 {
				continue
			}
			if !containsVariant(enumVariants, caseNode.CaseValue) {
				return "", fmt.Errorf("switch case references variant symbol %d, which is not one of the subject enum %s's declared variants", caseNode.CaseValue, enumTypeName(enumSubject))
			}
		}
	}
	// Group case nodes by shared body node ID to detect multi-value case
	// labels (a `case 1, 2:` clause produces two SwitchCase nodes sharing
	// one body node ID). Preserve encounter order within each group and
	// across groups.
	groups, err := groupSwitchCases(unit, switchNode.Children[1:])
	if err != nil {
		return "", err
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
			bodyText, err := buildSwitchCaseBodyOrFallthrough(st, unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%sdefault: %s", caseIndent, bodyWrap(bodyText)))
			continue
		}
		// Emit stacked case labels for each SwitchCase in the group.
		for _, caseID := range g.caseIDs {
			caseNode, _ := unit.Node(caseID)
			label, err := buildCaseLabel(snapshot, caseNode, subjectIntWidth)
			if err != nil {
				return "", err
			}
			parts = append(parts, fmt.Sprintf("%s%s", caseIndent, label))
		}
		// The body is shared across all cases in the group.
		bodyText, err := buildSwitchCaseBodyOrFallthrough(st, unit, snapshot, fileSet, g.bodyID, locals, depth+2, width, result, unions, fallThrough)
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
func buildSwitchCaseBodyOrFallthrough(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if fallThrough {
		return buildLoopSwitchCaseBody(st, unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	return buildSwitchCaseBody(st, unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
}

// switchCaseGroup groups one switch case arm's SwitchCase nodes by their shared
// body node ID (a multi-value `case v1, v2:` clause produces several SwitchCase
// nodes sharing one body), preserving encounter order within each group and
// across groups. The else/default arm is its own group with elseID set.
type switchCaseGroup struct {
	bodyID  tir.NodeID
	caseIDs []tir.NodeID
	elseID  tir.NodeID // non-zero if this group is the else/default arm
}

// groupSwitchCases groups a switch's SwitchCase children by shared body node
// ID to detect multi-value case labels. Each case node is validated for the
// expected kind and child count. The else arm's body is Children[0]; a non-else
// arm's body is also Children[0] (a SwitchCase with 1 child has the body
// directly; with 2 children the body is still Children[0], the second is
// unused defense — confirmed against real fixtures).
func groupSwitchCases(unit *tir.Unit, caseIDs []tir.NodeID) ([]switchCaseGroup, error) {
	groupByBody := make(map[tir.NodeID]int)
	var groups []switchCaseGroup
	for _, caseID := range caseIDs {
		caseNode, ok := unit.Node(caseID)
		if !ok {
			return nil, fmt.Errorf("switch statement references invalid case node %d", caseID)
		}
		if caseNode.Kind != tir.SwitchCase {
			return nil, fmt.Errorf("switch statement child is a %s, want a SwitchCase", caseNode.Kind)
		}
		if caseNode.HasElse {
			groups = append(groups, switchCaseGroup{bodyID: caseNode.Children[0], elseID: caseID})
			continue
		}
		if len(caseNode.Children) != 1 && len(caseNode.Children) != 2 {
			return nil, fmt.Errorf("switch case has %d child(ren), want 1 or 2 (the body block)", len(caseNode.Children))
		}
		bodyID := caseNode.Children[0]
		if idx, exists := groupByBody[bodyID]; exists {
			groups[idx].caseIDs = append(groups[idx].caseIDs, caseID)
		} else {
			idx := len(groups)
			groupByBody[bodyID] = idx
			groups = append(groups, switchCaseGroup{bodyID: bodyID, caseIDs: []tir.NodeID{caseID}})
		}
	}
	return groups, nil
}

// buildStrSwitchStatement validates and builds the C text for a switch whose
// subject is a str value. A str subject cannot use a native C switch (C switch
// labels must be integer-constant expressions), so the lowering is a chain of
// if / else if tests, one per case, each calling the runtime helper
// pebble_rt_str_eq (the exact helper buildComparison uses for a == between two
// str values) against the subject and the case's string literal. Multiple case
// labels sharing one arm (`case "a", "b":`) are ORed into a single if
// condition. The else/default arm becomes the chain's final else. Because the
// chain splices the subject's C text into every equality check, the subject is
// first materialized ONCE into a PebbleStr local temp (the same
// evaluate-once-into-a-per-operand-temp pattern the composite print operands
// use): a call-valued subject (switch choose() { ... }) must not run its
// observable side effects once per case comparison. The emitted text, indented
// at this switch's depth, is:
//
//	<indent>PebbleStr pebble_switch_str_<subjectNodeID> = <subject>;
//	<indent>if (pebble_rt_str_eq(pebble_switch_str_<subjectNodeID>, (PebbleStr){...})) {
//	<indent>    <body>
//	<indent>} else if (pebble_rt_str_eq(pebble_switch_str_<subjectNodeID>, (PebbleStr){...})) {
//	<indent>    <body>
//	<indent>} else {
//	<indent>    <body>
//	<indent>}
//
// Each arm body is built by the exact same buildSwitchCaseBodyOrFallthrough
// helper the native-switch path uses (buildSwitchCaseBody's every-arm-ends-in-
// return grammar for a tail-position switch, buildLoopSwitchCaseBody's
// may-fall-through grammar for a fall-through switch), so defer/return/
// break/continue semantics inside an arm are identical between the two
// lowerings. A fall-through switch's case body does NOT get the trailing
// `break;` the native C switch path adds: an if/else chain has no C
// fall-through into the next arm, so a body that simply ends terminates the
// switch by falling past the chain — the same behavior the native path's
// trailing break produces. When the switch's case bodies contain a tir.Break
// whose Target is the switch's own region (Pebble's break targets the nearest
// enclosing loop or switch), the whole chain is wrapped in a do { ... } while
// (0) block, the idiomatic C pattern that gives the emitted `break;` a valid
// enclosing loop/switch construct to target — the break then exits the chain,
// i.e. breaks out of the switch, exactly as it does in the native C switch.
func buildStrSwitchStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, switchNode tir.Node, subjectNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, fallThrough bool) (string, error) {
	if len(switchNode.Children) < 2 {
		return "", fmt.Errorf("switch statement has %d child(ren), want at least 2 (the subject and one case)", len(switchNode.Children))
	}
	// The str subject is built by the same buildStrOperand every other
	// str-typed position in this backend uses (a str local reference, a
	// string literal, or a call to a str-returning helper).
	subjectExpr, err := buildStrOperand(st, unit, snapshot, fileSet, switchNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	// Materialize the subject into a PebbleStr local temp exactly once,
	// before the if/else chain. The chain splices the subject's C text into
	// every pebble_rt_str_eq call below, so a call-valued subject (switch
	// choose() { ... }) would otherwise be evaluated once per case label —
	// every observable side effect of the call would run once per case
	// comparison instead of once total. The temp is the same
	// evaluate-once-into-a-per-operand-temp pattern the composite print
	// operands use: the subject expression appears exactly once, as the
	// temp's initializer, and every equality check below reads the temp.
	subjectTemp := fmt.Sprintf("pebble_switch_str_%d", switchNode.Children[0])
	subjectDecl := indent + fmt.Sprintf("PebbleStr %s = %s;", subjectTemp, subjectExpr)
	// Group case nodes by shared body node ID (multi-value case labels).
	groups, err := groupSwitchCases(unit, switchNode.Children[1:])
	if err != nil {
		return "", err
	}
	// Detect a break targeting this switch (a tir.Break whose Target names
	// the switch's region) anywhere in the case bodies: the if/else chain
	// has no enclosing native switch/loop for the emitted `break;` to
	// target, so the whole chain is wrapped in do { ... } while (0) — the
	// idiomatic C pattern giving break a valid target (it then exits the
	// chain = breaks the switch). A tail-position switch (fallThrough
	// false) cannot contain such a break: every arm ends in a return and
	// buildSwitchCaseBody rejects a non-return tail — so only a
	// fall-through switch ever needs the wrapper.
	needsDoWhile := false
	for _, g := range groups {
		found, valid := loopBodyHasBreakTargeting(unit, g.bodyID, switchNode.Region)
		if !valid {
			return "", fmt.Errorf("switch statement references invalid case body node %d", g.bodyID)
		}
		if found {
			needsDoWhile = true
		}
	}
	var lines []string
	for idx, g := range groups {
		bodyText, err := buildSwitchCaseBodyOrFallthrough(st, unit, snapshot, fileSet, g.bodyID, locals, depth+1, width, result, unions, fallThrough)
		if err != nil {
			return "", err
		}
		if g.elseID != 0 {
			header := "} else"
			if idx == 0 {
				header = "else"
			}
			lines = append(lines, fmt.Sprintf("%s%s {", indent, header))
			lines = append(lines, bodyText)
			continue
		}
		// One equality check per case label in the group; multiple labels
		// on one arm are ORed together into a single if condition.
		conds := make([]string, 0, len(g.caseIDs))
		for _, caseID := range g.caseIDs {
			caseNode, _ := unit.Node(caseID)
			lit, err := buildStrCaseLiteral(snapshot, caseNode)
			if err != nil {
				return "", err
			}
			conds = append(conds, "pebble_rt_str_eq("+subjectTemp+", "+lit+")")
		}
		header := "if"
		if idx > 0 {
			header = "} else if"
		}
		lines = append(lines, fmt.Sprintf("%s%s (%s) {", indent, header, strings.Join(conds, " || ")))
		lines = append(lines, bodyText)
	}
	lines = append(lines, indent+"}")
	chain := strings.Join(append([]string{subjectDecl}, lines...), "\n")
	if needsDoWhile {
		return fmt.Sprintf("%sdo {\n%s\n%s} while (0);", indent, chain, indent), nil
	}
	return chain, nil
}

// buildStrCaseLiteral emits the C text for one str switch case's literal: the
// case's decoded string as a PebbleStr compound literal,
// `(PebbleStr){ .data = (const uint8_t *)"<escaped>", .len = N }`. The
// literal text comes from the SwitchCase node's Literal.String field (set by the
// checker's constantToLiteral from constantString), the same decoded string
// buildStrLiteralValue produces for a StringLiteral node — reused here via the
// same compound-literal shape so every str value in the emitted C uses one
// uniform representation. A case node whose Literal is not LiteralString is a
// clean rejection (the checker only produces LiteralString cases for a str
// subject, confirmed against real fixtures).
func buildStrCaseLiteral(snapshot *types.Snapshot, caseNode tir.Node) (string, error) {
	if caseNode.Literal.Kind != tir.LiteralString {
		return "", fmt.Errorf("switch case has literal kind %s, want a string constant", caseNode.Literal.Kind)
	}
	text := caseNode.Literal.String
	return fmt.Sprintf("(PebbleStr){ .data = (const uint8_t *)\"%s\", .len = %d }", escapeCString(text), len(text)), nil
}

// buildCaseLabel emits one C `case <value>:` label from a SwitchCase node.
// An enum-variant case (CaseValue set — a CaseValue-based case, produced by
// the checker for an enum subject) is emitted as
// `case pebble_variant_<caseValue>:`, the variant's C enum constant, whose
// value (the variant's ordinal in the enum's declared order) matches the
// subject's own typedef by construction. An integer literal is emitted as its
// decimal text at the SUBJECT's own resolved integer width — the width
// parameter is the subject's fixed-width integer builtin (the entry's width
// for an entry-width subject, or the subject's own u8/i16/... width for a
// non-entry-width one), so an unsigned subject's labels get the same `u`
// suffix integerLiteralText gives every other unsigned value in the emitted
// C, keeping the case constant's C type aligned with the subject's C type
// (uint8_t gets `case 255u:`, never a silently unsigned/negative
// interpretation). A NEGATIVE integer literal (the checker's canonical
// big.Int text, a leading `-` followed by digits) is emitted as its negative
// decimal text on a SIGNED subject (`case -5:` at i16), and cleanly rejected
// on an unsigned subject, which has no representation for a negative value
// (accepting one would silently reinterpret it as a huge unsigned constant).
// A bool literal is emitted as `0` (false) or `1` (true), since C treats bool
// as an integer type and switch cases require integral constant expressions;
// a char literal is emitted as `case (int32_t)<scalar>:`, the same int32_t
// spelling buildCharOperand gives a char value everywhere, so the label
// matches a char-typed subject's integral C representation. Any other case
// shape is a clean rejection.
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
		if isNegativeDecimal(text) {
			// A negative case label is legal only on a signed subject: an
			// unsigned subject has no representation for a negative value, so
			// a `u`-suffixed negative C constant (`case -5u:`) would silently
			// reinterpret the literal as a huge unsigned value instead of
			// rejecting it. Reject cleanly at the subject's own width.
			if isUnsignedWidth(width) {
				name, _ := builtinName(width)
				return "", fmt.Errorf("switch case contains a negative integer literal %q on an unsigned subject type %s", text, name)
			}
			return "case " + integerLiteralText(text, width) + ":", nil
		}
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("switch case contains an integer literal with malformed text %q", text)
		}
		return "case " + integerLiteralText(text, width) + ":", nil
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
func buildSwitchCaseBody(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildBlock(st, unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions)
	}
	// Bare single-statement case body: must be a Return. Built by the shared
	// buildReturnStatement — the exact dispatch every other Return position in
	// this backend uses (bool, char, str, tuple/struct, union, enum, array,
	// slice, optional, function, float, uint, and a void-returning bare
	// `return;`) — not a partial re-dispatch. This path previously re-coded
	// only the char/str/aggregate/slice/float branches and fell through to
	// buildExpr for everything else, which silently routed a union variant
	// construction (`case .value: return C.value(5);`), a plain enum value, a
	// bool, an array, or an optional into buildExpr's integer-width gate
	// ("want int") even though the block-body and fall-through paths both
	// already dispatched these through buildReturnStatement.
	if bodyNode.Kind == tir.Return {
		return buildReturnStatement(st, unit, snapshot, fileSet, bodyNode, locals, strings.Repeat("    ", depth+1), "switch case body", width, result, unions)
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
func buildLoopSwitchCaseBody(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	bodyNode, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("switch case body references invalid node %d", bodyID)
	}
	if bodyNode.Kind == tir.Block {
		return buildFallthroughBody(st, unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "switch case body")
	}
	// Bare single-statement case body: built as one fall-through statement by
	// the same dispatch a statement inside a Block case body (or a loop body,
	// or an if arm) goes through, so it may be a Store, a call, a print, a
	// return, a nested if/switch, and so on — not just a Return.
	indent := strings.Repeat("    ", depth+1)
	return buildFallthroughStatement(st, unit, snapshot, fileSet, bodyID, locals, indent, depth, width, result, unions, "switch case body")
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
func buildIf(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if !ifNode.HasElse {
		return "", fmt.Errorf("entry function body ends with an if without an else; this backend only supports the two-armed if/else whose arms each end in one return, found an if with no else")
	}
	if len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body ends with an if with %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(st, unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildBlock(st, unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	elseText, err := buildBlock(st, unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
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
func buildWhile(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, whileNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if len(whileNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop has %d child(ren), want exactly 2 (the condition, then the loop body)", len(whileNode.Children))
	}
	condition, err := buildCondition(st, unit, snapshot, fileSet, whileNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	bodyText, err := buildLoopBody(st, unit, snapshot, fileSet, whileNode.Children[1], locals, depth+1, width, result, unions)
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
// <indent><cType> pebble_temp_<startNodeID> = <start>;
// <indent><cType> pebble_temp_<endNodeID> = <end>;
// <indent>int32_t pebble_step_<iterSym> = (pebble_temp_<startNodeID> <= pebble_temp_<endNodeID>) ? 1 : -1;
// <indent>for (<cType> pebble_local_<iterSym> = pebble_temp_<startNodeID>; (pebble_step_<iterSym> > 0) ? (pebble_local_<iterSym> < pebble_temp_<endNodeID>) : (pebble_local_<iterSym> > pebble_temp_<endNodeID>); pebble_local_<iterSym> += pebble_step_<iterSym>) {
// <loop body statements, one level deeper>
// <indent>}
//
// This is V1's actual production lowering (src/codegen.c, AST_STMT_LOOP)
// verbatim: the loop direction is computed at runtime from the two bounds'
// values rather than at compile time. Both bounds are evaluated exactly once,
// START first then END, into C locals (so a side-effecting or expensive bound
// runs once, in source order, not once per condition check); the step is
// computed once from comparing them; and the for-loop condition is a ternary
// on the step so one uniform shape handles ascending, descending,
// zero-length, negative-literal, and runtime-computed ranges identically —
// there is no compile-time literal detection at all. `<`/`>` for the exclusive
// form (`..`), `<=`/`>=` for the inclusive form (`..=`), from the node's
// RangeInclusive field. The iterator's own C type is the entry's
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
func buildRangeLoop(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, rangeNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
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
	startText, err := buildRangeBound(st, unit, snapshot, fileSet, rangeNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	endText, err := buildRangeBound(st, unit, snapshot, fileSet, rangeNode.Children[1], locals, width)
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
	bodyText, err := buildLoopBody(st, unit, snapshot, fileSet, rangeNode.Children[2], loopScope, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	// The loop direction is decided at runtime, exactly as V1's codegen does
	// (src/codegen.c, AST_STMT_LOOP): both bounds are evaluated once, START
	// first then END, into C locals declared before the loop; a step local is
	// computed once from comparing them; and the for-loop condition is a
	// ternary on the step. One uniform lowering therefore handles
	// compile-time-ascending, compile-time-descending, negative-literal, and
	// runtime-computed ranges identically, with no compile-time literal
	// detection at all — a range loop whose bounds are only known at runtime
	// (a call, a local, a checked negation of a literal) descends exactly as a
	// literal descending range does, instead of silently running zero
	// iterations under a hardcoded ascending condition. Materializing both
	// bounds into locals also makes each bound run exactly once (not once per
	// condition check) and in source order (start before end), the
	// evaluation-order the V1 lowering guarantees. The start/end locals are
	// declared at the loop's own bound type — the checker anchors the start
	// bound to the end bound's type, so boundType is both bounds' type and a
	// uint-bounded range (std/hmap.peb's `loop 0..new_cap : i { ... }`) keeps
	// its uint64_t locals and compares them against the uint64_t iterator
	// without tripping -Wsign-compare under the mandated -Wall -Wextra -Werror.
	// The step local is deliberately always signed: it holds only -1/+1, and
	// the condition branches on its sign, so an unsigned step would both trip
	// -Wsign-compare and wrap -1 to a huge positive value on a descending
	// unsigned range. The increment `i += step` needs no signedness care for
	// EXCLUSIVE ranges — the step's implicit conversion into the iterator's
	// type wraps the -1 down to the right value for unsigned iterators, and a
	// descending exclusive loop's last decrement is from end+1 down to end
	// (end == 0 means from 1 down to 0), never from 0 itself.
	//
	// The INCLUSIVE form is different: its last iteration IS the end bound, so
	// after the body runs at i == end the unconditional `i += step` advances
	// an unsigned iterator one past end — 0 - 1 wraps to UINT_MAX on a
	// descending range (and max + 1 wraps to 0 on an ascending one) — and the
	// step-ternary condition (i <= end / i >= end) then reads the wrapped
	// value as still in range forever. A literal/`int`-typed inclusive range
	// never hits this (signed one-past-end is representable), but V2 lets the
	// iterator be the bounds' own unsigned width, so `loop 5..=0 : i` over u8
	// wrapped to 255 and looped forever (reproduced). The fix is a done local
	// that is set, from the still-unincremented current value, inside the
	// for-increment's comma expression: `!done` replaces the step-ternary as
	// the condition (an inclusive range always visits its start bound at least
	// once, so there is no range check to keep), the increment sets done once
	// the current value is the end bound, and the following `i += step` may
	// then wrap with no consequence because the done flag has already ended
	// the loop. Placing the done test in the increment rather than as a
	// post-body break keeps a body `continue` correct: C's continue jumps
	// straight to the increment clause, which is exactly where the done test
	// lives, so a continue on the final iteration still terminates instead of
	// skipping a trailing break and wrapping into an infinite loop.
	indent := strings.Repeat("    ", depth+1)
	iterCType := cType(boundType)
	startTemp := fmt.Sprintf("pebble_temp_%d", rangeNode.Children[0])
	endTemp := fmt.Sprintf("pebble_temp_%d", rangeNode.Children[1])
	stepTemp := fmt.Sprintf("pebble_step_%d", rangeNode.Symbol)
	iter := fmt.Sprintf("pebble_local_%d", rangeNode.Symbol)
	var b strings.Builder
	fmt.Fprintf(&b, "%s%s %s = %s;\n", indent, iterCType, startTemp, startText)
	fmt.Fprintf(&b, "%s%s %s = %s;\n", indent, iterCType, endTemp, endText)
	fmt.Fprintf(&b, "%sint32_t %s = (%s <= %s) ? 1 : -1;\n", indent, stepTemp, startTemp, endTemp)
	if rangeNode.RangeInclusive {
		doneTemp := fmt.Sprintf("pebble_done_%d", rangeNode.Symbol)
		fmt.Fprintf(&b, "%sint32_t %s = 0;\n", indent, doneTemp)
		fmt.Fprintf(&b, "%sfor (%s %s = %s; !%s; %s |= (%s > 0) ? (%s >= %s) : (%s <= %s), %s += %s) {\n%s\n%s}", indent, iterCType, iter, startTemp, doneTemp, doneTemp, stepTemp, iter, endTemp, iter, endTemp, iter, stepTemp, bodyText, indent)
	} else {
		ascOp, descOp := "<", ">"
		fmt.Fprintf(&b, "%sfor (%s %s = %s; (%s > 0) ? (%s %s %s) : (%s %s %s); %s += %s) {\n%s\n%s}", indent, iterCType, iter, startTemp, stepTemp, iter, ascOp, endTemp, iter, descOp, endTemp, iter, stepTemp, bodyText, indent)
	}
	return b.String(), nil
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
func buildRangeBound(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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
		if name, ok := localOrGlobalName(st, node.Symbol, locals); ok {
			return name, nil
		}
		return "", fmt.Errorf("entry function body block range loop bound references symbol %d, which is not a local in scope", node.Symbol)
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
		return buildExpr(st, unit, snapshot, fileSet, id, locals, boundWidth, width)
	}
	if isUint(snapshot, node.Type) {
		return buildUintExpr(st, unit, snapshot, fileSet, id, locals, width)
	}
	return buildExpr(st, unit, snapshot, fileSet, id, locals, width, width)
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
// single Initialize declaring a local of the entry's width or bool or a
// single Store reassigning a local already in scope (built by
// buildForInitClause) — a CompoundStore/ExpressionStatement initializer
// (`for x += 1; ...`, `for x + 1; ...`) is reachable from real source and is
// a clean rejection, matching the backend's rule that only an Initialize
// declares a local and only a Store reassigns one. The update, when present, must be
// a single Store reassigning a local already in scope or a single CompoundStore
// (a compound assignment such as `step += 1` or a postfix `step++`, built by
// buildForUpdateClause through buildStoreCore / buildCompoundStore) — a
// discarded-expression update (`for x + 1; ...`) is reachable from real source
// and is a clean rejection. With no condition present the checker's fixed relative
// order leaves at most `[initializer?] [update?]`, so an Initialize or Store
// child is the initializer and a Store/CompoundStore child is the update; a lone
// no-condition Store is treated as the update (the in-scope update-only shape
// `for ; ; update {
// ... }`) — note this makes a no-condition Store *initializer* (`for step =
// 0;; { ... }`, out of scope but reachable) structurally indistinguishable
// from update-only and silently lowered as the update, a real ambiguity with
// no IR-level way to tell them apart (the For node carries only Region and
// Children). The condition is built by the exact same buildCondition an
// if/while condition uses. The body is built by the exact same buildLoopBody
// a while/range loop uses, one level deeper, against a cloned scope seeded
// with the initializer's local if the initializer declares one — so a
// SymbolValue reference to the initializer's local inside the condition,
// update, or body resolves through the existing machinery unchanged,
// mirroring how a range loop seeds its iterator; an assignment-form
// initializer declares nothing new, and the already-in-scope variable it
// reassigns is present in the cloned scope from the enclosing block. If the
// initializer declares a local, a `(void)
// pebble_local_<symbol>;` cast is emitted as the body's first statement, the
// same -Wunused-variable defense every declared local gets (confirmed: cc
// fires -Wunused-variable under -Wall -Wextra -Werror for a for-init local
// never referenced anywhere, and the cast is a no-op when it is); an
// assignment-form initializer declares nothing new, so no cast is emitted.
// The body is
// its own scope (buildLoopBody clones), so nothing the body declares leaks
// outside, while the seeded initializer local remains visible inside.
// break/continue inside the body are handled by buildLoopBody's own
// Break/Continue cases — plain C break/continue already target the nearest
// enclosing loop, which the emitted for loop is (the same confirmation a
// range loop made). Any other shape — an ambiguous clause list, an
// out-of-scope initializer or update, a missing or non-Block body — is a
// clean rejection naming what was found.
func buildFor(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, forNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
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
		// the initializer and must be an Initialize or a Store (built by
		// buildForInitClause); a nonvalue clause after
		// it is the update and must be a Store.
		if condIndex > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) before its condition, want at most one (the initializer)", condIndex)
		}
		if len(clauses)-condIndex-1 > 1 {
			return "", fmt.Errorf("entry function body block for loop has %d clause(s) after its condition, want at most one (the update)", len(clauses)-condIndex-1)
		}
		if condIndex == 1 {
			pre, text, symbol, err := buildForInitClause(st, unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
		}
		cond, err := buildCondition(st, unit, snapshot, fileSet, clauses[condIndex], loopScope, width)
		if err != nil {
			return "", err
		}
		condText = cond
		if len(clauses)-condIndex-1 == 1 {
			updateID = clauses[len(clauses)-1]
			pre, text, err := buildForUpdateClause(st, unit, snapshot, fileSet, clauses[len(clauses)-1], loopScope, width, unions)
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
				pre, text, symbol, err := buildForInitClause(st, unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
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
				pre, text, err := buildForUpdateClause(st, unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
				if err != nil {
					return "", err
				}
				updateText = text
				updatePre = pre
			case tir.CompoundStore:
				updateID = clauses[0]
				pre, text, err := buildForUpdateClause(st, unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
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
			if initClause.Kind != tir.Initialize && initClause.Kind != tir.Store {
				return "", fmt.Errorf("entry function body block for loop with no condition leads with a %s clause, want an Initialize (the initializer declares a local) or a Store (the initializer reassigns a local already in scope)", initClause.Kind)
			}
			if updateClause.Kind != tir.Store && updateClause.Kind != tir.CompoundStore {
				return "", fmt.Errorf("entry function body block for loop with no condition follows the initializer with a %s clause, want a Store or CompoundStore (the update)", updateClause.Kind)
			}
			pre, text, symbol, err := buildForInitClause(st, unit, snapshot, fileSet, clauses[0], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			initText = text
			initPre = pre
			initSymbol = symbol
			updateID = clauses[1]
			pre, text, err = buildForUpdateClause(st, unit, snapshot, fileSet, clauses[1], loopScope, width, unions)
			if err != nil {
				return "", err
			}
			updateText = text
			updatePre = pre
		default:
			return "", fmt.Errorf("entry function body block for loop with no condition has %d clause(s), want at most two (an initializer and an update)", len(clauses))
		}
	}
	bodyText, err := buildLoopBody(st, unit, snapshot, fileSet, bodyID, loopScope, depth+1, width, result, unions)
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
		lvalue, _, err := buildPlaceLValue(st, unit, snapshot, fileSet, updateNode.Children[0], loopScope, width)
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
// for loop's initializer. The initializer has two supported forms:
//
//   - A declaration form, `<cType> pebble_local_<symbol> = <expr>` — a C
//     declaration with no leading indent, no statement-terminating newline, and
//     no trailing `;` of its own (the for-header `for (<init>; <cond>;
//     <update>)`'s first `;` is what terminates the init clause). The
//     initializer must be a single Initialize declaring a local, of any
//     integer width (not just the entry's resolved width), bool, or char — the
//     same scalar grammars a bare Initialize supports — validated and emitted
//     by buildScalarInitializeCore, which also records the local in the
//     caller's loop scope so the condition, update, and body can reference it.
//     Returns the declared symbol (so buildFor can emit the (void) cast as the
//     body's first statement).
//   - An assignment form, `pebble_local_<symbol> = <expr>` — a plain
//     reassignment of a variable already in scope (declared before the loop),
//     the ordinary pattern of reusing an existing counter across loops or
//     seeding it from a value computed earlier. The clause text is the bare
//     assignment expression built by buildStoreCore's plain-local path against
//     the variable's own declared type, and symbol 0 is returned — there is
//     nothing new to (void)-cast, since the reassigned variable was already
//     declared and seeded into scope before the loop, so buildFor emits no
//     body-first cast for it. The place must name an already-declared symbol:
//     buildStoreCore resolves it against scope (a local in scope, a
//     module-level global, or an extern variable) and cleanly rejects a place
//     that resolves to nothing, which is the backend's existing "not in scope"
//     handling — a Store's place is guaranteed to name an in-scope symbol by
//     the checker.
//
// A CompoundStore or a discarded ExpressionStatement initializer remain
// reachable from real source but out of scope and cleanly rejected, matching
// the backend's rule that only an Initialize declares a local and only a Store
// reassigns one. For the declaration form, plus for an OptionalIntegerToEnum
// initializer, a pre statement text must be emitted BEFORE the for statement —
// the source integer's one-time-evaluation temp declaration, which a single
// for-header declaration cannot hold alongside the optional-typed local (the
// two have different C types, and a for-header declaration is a single C
// declaration) — mirroring the updatePre mechanism buildCompoundStore uses.
func buildForInitClause(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, string, symbol.SymbolID, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer references invalid node %d", id)
	}
	if statement.Kind == tir.Store {
		core, err := buildStoreCore(st, unit, snapshot, fileSet, statement, scope, "entry function body block for loop initializer", width, unions)
		if err != nil {
			return "", "", 0, err
		}
		return "", core, 0, nil
	}
	if statement.Kind != tir.Initialize {
		return "", "", 0, fmt.Errorf("entry function body block for loop initializer is a %s, want a local declaration (an Initialize) or an assignment (a Store); a for-loop initializer must declare a local of %s or bool, or reassign an already-declared one", statement.Kind, wantName(width))
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
		pre, core, err := buildOptionalIntegerToEnumDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", id, width)
		if err != nil {
			return "", "", 0, err
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return pre, core, statement.Symbol, nil
	}
	pre, core, err := buildScalarInitializeCore(st, unit, snapshot, fileSet, statement, initValue, scope, "entry function body block for loop initializer", width)
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
func buildForUpdateClause(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, unions map[types.TypeID]unionInfo) (string, string, error) {
	statement, ok := unit.Node(id)
	if !ok {
		return "", "", fmt.Errorf("entry function body block for loop update references invalid node %d", id)
	}
	if statement.Kind != tir.Store && statement.Kind != tir.CompoundStore {
		return "", "", fmt.Errorf("entry function body block for loop update is a %s, want a Store (a reassignment of a local already in scope) or a CompoundStore (a compound assignment or postfix increment/decrement); a for-loop update must be a single assignment", statement.Kind)
	}
	if statement.Kind == tir.CompoundStore {
		return buildCompoundStore(st, unit, snapshot, fileSet, id, statement, scope, "entry function body block for loop update", width)
	}
	core, err := buildStoreCore(st, unit, snapshot, fileSet, statement, scope, "entry function body block for loop update", width, unions)
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
func buildLoopBody(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	return buildFallthroughBody(st, unit, snapshot, fileSet, bodyID, locals, depth, width, result, unions, "entry function body block while loop body")
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
func buildFallthroughBody(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
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
		text, err := buildFallthroughStatement(st, unit, snapshot, fileSet, childID, scope, indent, depth, width, result, unions, context)
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
func buildFallthroughStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, childID tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
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
		text, err = buildWhile(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.RangeLoop:
		// A nested range loop inside a fall-through sequence (a loop body, an
		// if arm, or a switch case body) reuses buildRangeLoop unchanged: it
		// recurses into this same builder for its own body, so nested range
		// loops compose exactly like nested whiles do.
		text, err = buildRangeLoop(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.For:
		// A nested classic for loop inside a fall-through sequence reuses
		// buildFor unchanged: it recurses into this same builder for its own
		// body, so nested classic for loops compose exactly like nested whiles
		// and range loops do.
		text, err = buildFor(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.If:
		// A conditional statement inside a fall-through sequence is built by
		// buildLoopIf: its arms are themselves fall-through sequences (no
		// required tail, optional else), genuinely different from the
		// tail-requiring buildIf. Because buildLoopIf recurses into
		// buildLoopBody for each arm, a break or continue inside an arm is
		// handled by this same switch, unchanged.
		text, err = buildLoopIf(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Switch:
		// A switch statement inside a fall-through sequence is built by
		// buildLoopSwitch: its case bodies are themselves fall-through
		// sequences (may return or fall through), unlike the tail-requiring
		// buildSwitch. Because buildLoopSwitch recurses into this same
		// dispatch for each case body, a break or continue inside a case body
		// is handled here, unchanged — C's own break/continue scoping resolves
		// it to the nearest enclosing loop or switch, which matches Pebble's
		// break-target rules (see buildLoopSwitch).
		text, err = buildLoopSwitch(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
	case tir.Return:
		// A return inside a fall-through sequence — an if arm, a switch case
		// body, or a loop body — exits the enclosing function immediately,
		// built by the same shared buildReturnStatement the block-tail Return
		// case uses.
		text, err = buildReturnStatement(st, unit, snapshot, fileSet, statement, scope, indent, context, width, result, unions)
	case tir.Break:
		text, err = buildLoopJump(st, unit, snapshot, fileSet, statement, "break", indent, context, scope, width, result, unions)
	case tir.Continue:
		text, err = buildLoopJump(st, unit, snapshot, fileSet, statement, "continue", indent, context, scope, width, result, unions)
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
		text, err = buildLeadingStatement(st, unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	case tir.Print:
		// A print statement — `print a, b;` on its own line — flows through
		// the same shared leading-statement builder buildBlock uses, so the
		// emission logic lives in exactly one place.
		text, err = buildLeadingStatement(st, unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
	default:
		text, err = buildLeadingStatement(st, unit, snapshot, fileSet, childID, scope, indent, depth, context, width, result, unions)
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
func buildLoopIf(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block while loop body if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop body if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(st, unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildLoopBody(st, unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildLoopBody(st, unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions)
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
func buildLeadingIf(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo, context string) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("%s if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", context, len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("%s if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", context, len(ifNode.Children))
	}
	condition, err := buildCondition(st, unit, snapshot, fileSet, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildFallthroughBody(st, unit, snapshot, fileSet, ifNode.Children[1], locals, depth+1, width, result, unions, context+" arm")
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildFallthroughBody(st, unit, snapshot, fileSet, ifNode.Children[2], locals, depth+1, width, result, unions, context+" arm")
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
// (the built-in), a bare discarded-expression statement that is a call to a
// void-returning function (defer helper();, built by the same
// buildExpressionStatement the leading-statement case uses), and — since 10.47 —
// a deferred local declaration, in either its bare form (`defer var x = 5;`, a
// tir.Initialize) or its block form (`defer { var x = 5; print x; }`, a
// tir.Block). A declaration's initializer (or a deferred block's statements)
// runs at exit inside a fresh C block — V1's `{ /* defer */ ... }` defer-local
// block — built over a cloned scope that is discarded afterwards, so the
// deferred local is scoped to the defer and invisible outside it, while the
// block's own statements can still reference it and the enclosing locals. The
// emitted declaration block uses the shared buildLeadingStatement /
// buildFallthroughBody machinery the non-deferred declaration and fall-through
// positions use, so the emission logic lives in exactly one place. A
// DeferRegister whose child is an unsupported
// statement kind is a clean rejection naming what was found. result is the
// enclosing callable's resultInfo, threaded to the shared builders in case a
// deferred statement sequence ever contains a return (real source can't — C0613
// — but the builders need the value to build one correctly if a hand-built unit
// somehow reaches them).
func buildDeferredStatements(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, chain []tir.NodeID, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
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
			core, err := buildStoreCore(st, unit, snapshot, fileSet, stmt, scope, context, width, unions)
			if err != nil {
				return "", err
			}
			parts = append(parts, indent+core+";")
		case tir.CompoundStore:
			// A deferred compound assignment or postfix increment/decrement —
			// `defer i += 1;` — built by the same shared buildCompoundStore a
			// non-deferred compound assignment uses, so the emission logic
			// lives in exactly one place.
			pre, core, err := buildCompoundStore(st, unit, snapshot, fileSet, deferReg.Children[0], stmt, scope, context, width)
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
			text, err := buildExpressionStatement(st, unit, snapshot, fileSet, stmt, scope, indent, context, width)
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
			pre, text, err := buildPrint(st, unit, snapshot, fileSet, stmt, scope, indent, context, width)
			if err != nil {
				return "", err
			}
			if pre != "" {
				parts = append(parts, pre)
			}
			parts = append(parts, text)
		case tir.Initialize:
			// A bare deferred local declaration — `defer var x = 5;` — runs
			// its initializer at exit inside a fresh C block (V1's defer-local
			// block), built by the same buildLeadingStatement a non-deferred
			// local declaration uses over a cloned scope that is discarded
			// afterwards: the deferred local is visible only inside that block
			// (the checker scopes the binding to the defer), never to the
			// enclosing function's emission scope, so its C name can never
			// collide with a same-named local in the enclosing body and the
			// declaration's own (void) cast keeps -Wunused-variable quiet.
			if len(stmt.Children) != 1 {
				return "", fmt.Errorf("%s deferred local declaration initializes %d value(s), want exactly one expression", context, len(stmt.Children))
			}
			deferredScope := cloneLocals(scope)
			innerIndent := indent + "    "
			decl, err := buildLeadingStatement(st, unit, snapshot, fileSet, deferReg.Children[0], deferredScope, innerIndent, len(indent)/4, context+" deferred local declaration", width, result, unions)
			if err != nil {
				return "", err
			}
			parts = append(parts, indent+"{\n"+decl+"\n"+indent+"}")
		case tir.Block:
			// A deferred block — `defer { var x = 5; print x; }` — runs its
			// statements at exit inside a fresh C block (V1's defer-local
			// block), built by the same buildFallthroughBody a loop body or if
			// arm uses over a cloned scope that is discarded afterwards: a
			// local declared inside the block is visible to the block's own
			// later statements and nested constructs but invisible outside it,
			// exactly as the enclosing function's locals remain visible inside
			// it. A deferred block can only contain fall-through statements —
			// the checker's C0613 rejects a deferred return/break/continue/
			// nested defer, while a break/continue targeting a loop contained
			// inside the block is handled by buildLoopJump exactly as it is in
			// any fall-through sequence.
			deferredScope := cloneLocals(scope)
			inner, err := buildFallthroughBody(st, unit, snapshot, fileSet, deferReg.Children[0], deferredScope, len(indent)/4, width, result, unions, context+" deferred statement block")
			if err != nil {
				return "", err
			}
			if inner == "" {
				parts = append(parts, indent+"{}")
				continue
			}
			parts = append(parts, indent+"{\n"+inner+"\n"+indent+"}")
		default:
			return "", fmt.Errorf("%s deferred statement is a %s, which is not a supported deferred statement kind (only Store reassignment, a CompoundStore compound assignment or postfix increment/decrement, a void-returning function call used as a statement, and a local declaration, bare or block-wrapped, are supported)", context, stmt.Kind)
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
func buildLoopJump(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, keyword string, indent, context string, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
	deferText, err := buildDeferredStatements(st, unit, snapshot, fileSet, statement.DeferChain, scope, indent, context, width, result, unions)
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
func buildLeadingStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, id tir.NodeID, scope map[symbol.SymbolID]localInfo, indent string, depth int, context string, width types.BuiltinKind, result resultInfo, unions map[types.TypeID]unionInfo) (string, error) {
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
			return buildTupleLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isArray(snapshot, initValue.Type) {
			if initValue.Kind == tir.DirectCall {
				return buildArrayCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			return buildArrayLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isOptional(snapshot, initValue.Type) {
			// An optional-typed local: its type is the initializer value's
			// Type (the Initialize node carries no Type itself, confirmed
			// against a real fixture — same as tuple/array locals). The
			// supported initializers are SomeOptional (some <expr>), none, and
			// since the OptionalIntegerToEnum slice an integer-to-optional-
			// enum cast (`5 as ?Color`); every other optional initializer
			// shape is a clean rejection.
			return buildOptionalLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width, id)
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
			if initValue.Kind == tir.DirectCall || initValue.Kind == tir.MethodCall {
				// A call to an enum/union-returning helper used as the direct
				// initializer of a matching enum/union-typed local —
				// `let c Color = pick();` — the call-site half of the
				// enum/union helper-return support (see
				// buildEnumCallInitializer / buildUnionCallInitializer).
				if _, isUnion := unions[initValue.Type]; isUnion {
					return buildUnionCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
				}
				return buildEnumCallInitializer(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			if _, isUnion := unions[initValue.Type]; isUnion {
				return buildUnionLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, unions, width)
			}
			return buildEnumLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStruct(snapshot, initValue.Type) {
			if runtimeType(unit, snapshot, initValue.Type) != 0 {
				return buildRuntimeLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
			}
			// A struct-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as tuple/array/optional locals). The
			// supported initializer is a RecordConstruct (a struct literal);
			// every other struct initializer shape is a clean rejection.
			return buildStructLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isStr(snapshot, initValue.Type) {
			// A str-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above). The
			// supported initializer is a StringLiteral (a string literal), a
			// call to a str-returning helper (a DirectCall whose result type
			// is str, since 10.36), a SymbolValue naming an in-scope str
			// local (a whole-str copy), or a Load of a str tuple element /
			// str struct field (a whole-str read-back); every other str
			// initializer shape is a clean rejection.
			return buildStrLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
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
			return buildSliceLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isPointer(snapshot, initValue.Type) {
			// A pointer-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializers are an AddressOf
			// expression (`let p *i32 = &y;`), another pointer-typed local
			// (pointer-to-pointer copy), a nil literal, a pointer-returning
			// call, or an explicit pointer-to-pointer cast; every other
			// pointer initializer shape is a clean rejection.
			return buildPointerLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		if isFunctionType(snapshot, initValue.Type) {
			// A function-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, same as every other
			// compound local). The supported initializer is a function value —
			// a bare top-level function reference (a HoistedFunctionValue) or
			// another function-typed local (a SymbolValue); every other
			// function initializer shape is a clean rejection (see
			// buildFunctionLocalDeclaration).
			return buildFunctionLocalDeclaration(st, unit, snapshot, fileSet, statement, initValue, scope, indent, context, width)
		}
		pre, core, err := buildScalarInitializeCore(st, unit, snapshot, fileSet, statement, initValue, scope, context, width)
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
		core, err := buildStoreCore(st, unit, snapshot, fileSet, statement, scope, context, width, unions)
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
		pre, core, err := buildCompoundStore(st, unit, snapshot, fileSet, id, statement, scope, context, width)
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
		return buildExpressionStatement(st, unit, snapshot, fileSet, statement, scope, indent, context, width)
	case tir.Print:
		// A print statement — `print a, b, c;` — emitted as one combined
		// printf call by the shared buildPrint (also used by buildLoopBody's
		// explicit Print case and buildDeferredStatements for a deferred
		// print), so the emission logic lives in exactly one place. A
		// slice-index operand's temp declaration is returned as a leading
		// pre-statement and threaded into this statement sequence before the
		// printf line, exactly as a return threads its pre-return temp.
		pre, line, err := buildPrint(st, unit, snapshot, fileSet, statement, scope, indent, context, width)
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
		return buildLeadingIf(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions, context)
	case tir.Switch:
		// A switch statement as an ordinary leading statement — a non-tail
		// switch whose case bodies may fall through or return, built by
		// buildLoopSwitch (the same fall-through switch the loop-body/arm
		// position uses). This is the only place a top-level function body can
		// contain a non-tail switch; see buildLoopSwitch.
		return buildLoopSwitch(st, unit, snapshot, fileSet, statement, scope, depth, width, result, unions)
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
func buildExpressionStatement(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if len(statement.Children) != 1 {
		return "", fmt.Errorf("%s discarded-expression statement has %d child(ren), want exactly one (the expression being discarded)", context, len(statement.Children))
	}
	expr, ok := unit.Node(statement.Children[0])
	if !ok {
		return "", fmt.Errorf("%s discarded-expression statement references invalid value node %d", context, statement.Children[0])
	}
	if expr.Kind == tir.IndirectCall {
		callExpr, err := buildIndirectCall(st, unit, snapshot, fileSet, expr, scope, width)
		if err != nil {
			return "", err
		}
		return indent + callExpr + ";", nil
	}
	if expr.Kind != tir.DirectCall && expr.Kind != tir.MethodCall {
		return "", fmt.Errorf("%s discarded-expression statement discards a %s, which is not supported as a bare statement yet (only a direct, method, or indirect call is)", context, expr.Kind)
	}
	callPre, callExpr, err := buildDirectCallWithPre(st, unit, snapshot, fileSet, expr, scope, width)
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
// operand independently type-checked). For an all-scalar print the emission
// matches v1's print codegen shape exactly: ONE combined printf call per
// print statement, not one per operand — every operand's format specifier is
// concatenated into a single format string (ending in the literal `\n`, so
// every print statement produces exactly one line of output) and every
// operand's value is a single argument, in the same order. A struct operand
// (composite print slice 1 — a struct whose fields are all scalar types) does
// not fold into that single call: it is emitted as DIRECT SEQUENTIAL
// `fprintf(stdout, ...)` calls, one per punctuation/label and one per field
// value, with the struct's declared type name and declared field names as the
// labels — proposal 17's storage policy (no intermediate dynamic string, so
// no dependency on the unfinished Allocator/Context redesign). When ANY
// operand of a print is a struct, the WHOLE statement is emitted that way, so
// a mixed `print p, 42;` stays one line of output; an all-scalar print keeps
// the combined-printf shape unchanged. The checker already restricts print operands to
// exactly bool, char, str, any integer builtin, any float builtin (C0612 — a
// nominal enum operand like `print Color.red;` is rejected upstream), or —
// composite print slice 1 — a struct value whose fields are all such scalars,
// so
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
func buildPrint(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	operands, hasComposite, err := unwrapPrintOperands(unit, snapshot, statement, context)
	if err != nil {
		return "", "", err
	}
	// A composite operand (a struct, a tuple, or a fixed array) cannot fold
	// into the single combined printf the way a scalar operand does — its
	// elements/fields print as direct sequential fprintf calls with
	// punctuation and labels — so when any operand is composite, the whole
	// print statement is emitted that way (see buildSequentialPrint). An
	// all-scalar print keeps the pre-existing single-combined-printf shape,
	// exactly as the tests that assert that shape expect.
	if hasComposite {
		return buildSequentialPrint(st, unit, snapshot, fileSet, statement, operands, scope, indent, context, width)
	}
	var formatParts []string
	var args []string
	var preParts []string
	for _, operandID := range operands {
		child, ok := unit.Node(operandID)
		if !ok {
			return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, operandID)
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
					boolExpr, err := buildBoolExpr(st, unit, snapshot, fileSet, part.Value, scope, width)
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
		format, arg, pres, err := buildScalarPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, width, context)
		if err != nil {
			return "", "", err
		}
		for _, pre := range pres {
			preParts = append(preParts, indent+pre)
		}
		formatParts = append(formatParts, format)
		args = append(args, arg)
	}
	line := indent + "printf(" + strings.Join(formatParts, "") + `"\n"`
	if len(args) != 0 {
		line += ", " + strings.Join(args, ", ")
	}
	return strings.Join(preParts, "\n"), line + ");", nil
}

// unwrapPrintOperands returns each print operand's innermost node ID after
// peeling off the SourceAlias grouping wrappers a parenthesized operand —
// `print ("hi")` — arrives in (one SourceAlias per grouping level, confirmed
// against a real fixture dump; a SourceAlias records grouped-expression parens
// and nothing else). Unwrapping here keeps the per-type value builders
// untouched (buildExpr/buildBoolExpr/buildFloatExpr unwrap a SourceAlias
// themselves, but buildCharOperand and buildStrOperand have no SourceAlias
// case); the unwrapped node carries the same Type the SourceAlias did, so the
// dispatch that follows is exactly what the checker validated. The bool
// reports whether any operand is a composite value — a struct (composite
// print slice 1), a tuple, or a fixed array (composite print slice 2) — which
// switches the whole print statement to the direct-sequential-fprintf
// emission (buildSequentialPrint).
func unwrapPrintOperands(unit *tir.Unit, snapshot *types.Snapshot, statement tir.Node, context string) ([]tir.NodeID, bool, error) {
	operands := make([]tir.NodeID, 0, len(statement.Children))
	hasComposite := false
	for _, childID := range statement.Children {
		child, ok := unit.Node(childID)
		if !ok {
			return nil, false, fmt.Errorf("%s print statement references invalid operand node %d", context, childID)
		}
		operandID := childID
		for child.Kind == tir.SourceAlias {
			if len(child.Children) != 1 {
				return nil, false, fmt.Errorf("%s print operand is a SourceAlias with %d child(ren), want exactly one", context, len(child.Children))
			}
			operandID = child.Children[0]
			child, ok = unit.Node(operandID)
			if !ok {
				return nil, false, fmt.Errorf("%s print statement references invalid operand node %d", context, operandID)
			}
		}
		operands = append(operands, operandID)
		if isStruct(snapshot, child.Type) && !isEnumType(unit, snapshot, child.Type) {
			hasComposite = true
		}
		if isTuple(snapshot, child.Type) || isArray(snapshot, child.Type) || isSlice(snapshot, child.Type) {
			hasComposite = true
		}
		// A plain enum or tagged-union operand is also composite for routing
		// purposes (composite print slices 5 and 6): it cannot fold into the
		// combined printf the way a scalar does, because its output requires a
		// runtime tag comparison to pick the variant name, not a static format
		// specifier — even though a plain enum has no nested fields to recurse
		// into (a union recurses into its variant payloads). The isEnumType
		// guard above already excludes an enum/union from the struct branch
		// (a union enum also passes isEnumType), so this line is what routes
		// it to buildSequentialPrint.
		if isEnumType(unit, snapshot, child.Type) {
			hasComposite = true
		}
	}
	return operands, hasComposite, nil
}

// buildScalarPrintOperand builds the printf pieces for one scalar print
// operand — the format-specifier piece, the printf/fprintf argument, and any
// pre-statements (a char operand's UTF-8 buffer) — by building the value's C
// expression under the grammar its own resolved type selects, exactly as the
// all-scalar combined-printf path does, and then formatting it. A slice-index
// operand (`print view()[0];`) routes its element read through
// buildSliceIndexValue exactly as before, with the base materialization temp
// returned as a leading pre-statement. The pieces are type-agnostic enough for
// both emission shapes to consume: the combined-printf path concatenates
// formats/args, the direct-sequential-fprintf path wraps each in its own
// fprintf call. Scalar formatting is NEVER reimplemented here — it lives in
// buildScalarPrintParts, the single formatting site a struct field's scalar
// value shares with a bare scalar operand.
func buildScalarPrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, width types.BuiltinKind, context string) (string, string, []string, error) {
	kind, ok := resolvedBuiltin(snapshot, child.Type)
	if !ok {
		return "", "", nil, fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
	}
	// A bare CheckedIndex whose base is a SLICE-typed value (not a str) —
	// `print view()[0];`, indexing a call's slice result directly. The indexed
	// element read needs the base materialized into a temp local (see
	// buildSliceIndexValue), whose temp-declaration statement this print
	// position — a statement sequence — hosts as a leading pre-statement,
	// exactly as buildReturnStatement threads a pre-return temp. A str-index
	// base (char result) stays on buildCharOperand's own CheckedIndex case
	// below.
	isSliceIndex := false
	if child.Kind == tir.CheckedIndex {
		strBase, err := checkedIndexBaseIsStr(unit, snapshot, child)
		if err != nil {
			return "", "", nil, err
		}
		isSliceIndex = !strBase
	}
	var expr string
	var sliceIndexPre string
	var err error
	switch {
	case cType(kind) != "":
		// An integer operand of any builtin width, not just the entry's own:
		// its value is built by buildExpr at its own resolved kind (re-checking
		// every node in the expression carries that width, exactly as a scalar
		// local declaration does). A slice-index operand's read (element C
		// type) carries the same width as its element.
		if isSliceIndex {
			sliceIndexPre, expr, err = buildSliceIndexValue(st, unit, snapshot, fileSet, operandID, child, scope, width, false)
		} else {
			expr, err = buildExpr(st, unit, snapshot, fileSet, operandID, scope, kind, width)
		}
	case kind == types.Bool:
		if isSliceIndex {
			sliceIndexPre, expr, err = buildSliceIndexValue(st, unit, snapshot, fileSet, operandID, child, scope, width, true)
		} else {
			expr, err = buildBoolExpr(st, unit, snapshot, fileSet, operandID, scope, width)
		}
	case kind == types.Char:
		if isSliceIndex {
			sliceIndexPre, expr, err = buildSliceIndexValue(st, unit, snapshot, fileSet, operandID, child, scope, width, false)
		} else {
			expr, err = buildCharOperand(st, unit, snapshot, fileSet, operandID, scope, width)
		}
	case kind == types.Str:
		expr, err = buildStrOperand(st, unit, snapshot, fileSet, operandID, scope, width)
	case kind == types.F32 || kind == types.F64:
		expr, err = buildFloatExpr(st, unit, snapshot, fileSet, operandID, scope, kind, width)
	default:
		return "", "", nil, fmt.Errorf("%s print operand is a %s of type %s, want bool, char, str, an integer, or a float", context, child.Kind, describeType(snapshot, child.Type))
	}
	if err != nil {
		return "", "", nil, err
	}
	format, arg, parts, err := buildScalarPrintParts(kind, expr, operandID, "")
	if err != nil {
		return "", "", nil, err
	}
	if sliceIndexPre != "" {
		return format, arg, append([]string{sliceIndexPre}, parts...), nil
	}
	return format, arg, parts, nil
}

// buildScalarPrintParts formats ONE scalar value whose C expression is already
// built (a bare print operand, or one scalar struct field read off a printed
// struct's temp) into the printf pieces: the format-specifier piece (with the
// exact-width <inttypes.h> PRI* macro spelled OUTSIDE the string quotes as
// `"%"PRId32`, so the preprocessor expands the macro and the adjacent literals
// concatenate — never `"%PRId32"`, a literal invalid `%P` specifier), the
// printf argument, and any pre-statements the value needs (a char value's
// pebble_rt_char_to_utf8 encoding into a fresh per-value uint8_t[5] buffer).
// This is the SINGLE scalar-formatting site: the combined-printf path, the
// direct-sequential-fprintf path, and a struct's scalar fields all route
// through it, so the exact formatting every existing scalar print already
// uses — integer PRI* macros, "true"/"false", the UTF-8 char conversion, the
// str .data projection, %f floats — is never duplicated. bufferPath
// disambiguates a char field's buffer name from a bare operand's: a bare
// operand (bufferPath "") names its buffer from the operand's node ID alone,
// a composite field/element appends its position path (e.g. "0_2" for field 0
// then element 2 of a nested aggregate) so every char field of a print
// operand — across any nesting depth — gets a distinct buffer.
func buildScalarPrintParts(kind types.BuiltinKind, expr string, operandID tir.NodeID, bufferPath string) (string, string, []string, error) {
	switch {
	case cType(kind) != "":
		// The format specifier comes from the <inttypes.h> PRI* macros whose
		// expansion matches the value's fixed-width C type — matching the
		// mandated -Wall -Wextra -Werror build's -Wformat-clean requirement.
		return `"%"` + printfSpecifier(kind), expr, nil, nil
	case kind == types.Bool:
		// A bool value prints as the words true/false: the value expression
		// wrapped in the C ternary that selects the const char * literal, so
		// the %s specifier's argument is already the pointer the format string
		// wants — v1's own approach for bool in print.
		return `"%s"`, "(" + expr + ` ? "true" : "false")`, nil, nil
	case kind == types.Char:
		// A char value prints as the UTF-8 encoding of the single character
		// its int32_t scalar value encodes. The scalar is never passed to
		// printf directly: a %c writes only a single byte, so any char beyond
		// U+007F would print corrupt bytes instead of its full UTF-8 sequence;
		// the runtime helper pebble_rt_char_to_utf8 encodes it — 1-4 UTF-8
		// bytes plus the trailing NUL — into a fresh per-value uint8_t[5]
		// buffer, and the %s specifier consumes that buffer. Routing every
		// char value — ASCII included — through the helper keeps the emitted C
		// uniform.
		bufferName := fmt.Sprintf("pebble_char_utf8_%d", operandID)
		if bufferPath != "" {
			bufferName = fmt.Sprintf("pebble_char_utf8_%d_%s", operandID, bufferPath)
		}
		return `"%s"`, "(const char *)" + bufferName, []string{
			fmt.Sprintf("uint8_t %s[5];", bufferName),
			fmt.Sprintf("pebble_rt_char_to_utf8(%s, %s);", expr, bufferName),
		}, nil
	case kind == types.Str:
		// A str value prints its bytes: the %s argument is the value's .data
		// field cast to const char * (the reachable str values this backend
		// builds all originate from NUL-terminated C string literals, so %s
		// reads exactly the intended bytes).
		return `"%s"`, "(const char *)" + expr + ".data", nil, nil
	case kind == types.F32 || kind == types.F64:
		// A float value prints with %f; f32/f64 promote to double in a
		// variadic call either way, so the one specifier covers both, matching
		// v1.
		return `"%f"`, expr, nil, nil
	}
	if name, ok := builtinName(kind); ok {
		return "", "", nil, fmt.Errorf("print value of builtin kind %s is not a printable scalar", name)
	}
	return "", "", nil, fmt.Errorf("print value of builtin kind %d is not a printable scalar", kind)
}

// printFprintfCall is one emitted `fprintf(stdout, ...)` call: a static
// format-string piece and the argument list that feeds it. A label call has no
// argument (`fprintf(stdout, "Point{ x: ");`); a value call carries the value
// expression built for the field or scalar operand. raw is the ONE case that is
// not a fprintf call at all: a fully-rendered literal C statement block (a
// slice's runtime for-loop, composite print slice 4) whose lines are all
// pre-indented, emitted verbatim instead of a call. When raw is set, format and
// args are ignored.
type printFprintfCall struct {
	format string
	args   []string
	raw    string
}

// text renders the call as one indented C statement. A raw entry's lines are
// already fully indented when the entry was built (buildSlicePrintValueCalls
// bakes the indent in), so it is emitted verbatim; every other entry renders as
// `fprintf(stdout, <format>, <args>);` (or `fprintf(stdout, <format>);` for a
// label call with no argument).
func (c printFprintfCall) text(indent string) string {
	if c.raw != "" {
		return c.raw
	}
	if len(c.args) == 0 {
		return indent + "fprintf(stdout, " + c.format + ");"
	}
	return indent + "fprintf(stdout, " + c.format + ", " + strings.Join(c.args, ", ") + ");"
}

// buildSequentialPrint emits a print statement that contains at least one
// composite operand (a struct — composite print slice 1 — a tuple, or a fixed
// array — composite print slice 2 — or a slice — composite print slice 4) as
// DIRECT SEQUENTIAL fprintf(stdout, ...) calls (proposal 17's storage policy
// for composite output: no intermediate dynamic string, so no dependency on the
// unfinished Allocator/Context redesign). Each operand contributes its calls in
// source order — a scalar operand exactly one call, a composite operand one
// punctuation/label/value call per element or field — and the very last call's
// format string carries the print's one trailing `\n`, so the whole statement
// still produces exactly one line of output. Every scalar value, every scalar
// struct field, and every scalar tuple/array/slice element is formatted by the
// SAME buildScalarPrintParts the all-scalar combined-printf path uses; nothing
// is reimplemented. A composite operand's value is materialized once into a
// per-operand temp local (pebble_print_struct_<nodeID> for a struct,
// pebble_print_tuple_<nodeID> for a tuple, pebble_print_array_<nodeID> for an
// array, pebble_print_slice_<nodeID> for a slice) so a composite-returning call
// operand is evaluated exactly once, and every field/element is then read off
// the temp. A slice's length is a RUNTIME value, so a slice operand cannot
// contribute a compile-time-known list of per-element calls; instead its
// operand builder returns one raw pre-rendered C for-loop entry (a printFprintf
// Call whose raw field is set) alongside its static `[` and `]` calls — see
// buildSlicePrintValueCalls.
func buildSequentialPrint(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, statement tir.Node, operands []tir.NodeID, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, string, error) {
	var preParts []string
	var calls []printFprintfCall
	for _, operandID := range operands {
		child, ok := unit.Node(operandID)
		if !ok {
			return "", "", fmt.Errorf("%s print statement references invalid operand node %d", context, operandID)
		}
		if child.Kind == tir.InterpolatedString {
			for _, part := range child.Parts {
				switch part.Kind {
				case tir.InterpolationTextPart:
					calls = append(calls, printFprintfCall{format: `"%s"`, args: []string{`(const char *)"` + escapeCString(part.Text) + `"`}})
				case tir.InterpolationValuePart:
					valueNode, ok := unit.Node(part.Value)
					if !ok {
						return "", "", fmt.Errorf("%s interpolated-string print operand references invalid value node %d", context, part.Value)
					}
					valueKind, ok := resolvedBuiltin(snapshot, valueNode.Type)
					if !ok || valueKind != types.Bool {
						return "", "", fmt.Errorf("%s interpolated-string print operand interpolates a %s of type %s, want bool", context, valueNode.Kind, describeType(snapshot, valueNode.Type))
					}
					boolExpr, err := buildBoolExpr(st, unit, snapshot, fileSet, part.Value, scope, width)
					if err != nil {
						return "", "", err
					}
					calls = append(calls, printFprintfCall{format: `"%s"`, args: []string{"(" + boolExpr + ` ? "true" : "false")`}})
				default:
					return "", "", fmt.Errorf("%s interpolated-string print operand has an unknown part kind %d", context, part.Kind)
				}
			}
			continue
		}
		if _, ok := resolvedBuiltin(snapshot, child.Type); ok {
			format, arg, pres, err := buildScalarPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, width, context)
			if err != nil {
				return "", "", err
			}
			for _, pre := range pres {
				preParts = append(preParts, indent+pre)
			}
			calls = append(calls, printFprintfCall{format: format, args: []string{arg}})
			continue
		}
		var compositeCalls []printFprintfCall
		var pres []string
		var err error
		switch {
		case isTuple(snapshot, child.Type):
			compositeCalls, pres, err = buildTuplePrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		case isArray(snapshot, child.Type):
			compositeCalls, pres, err = buildArrayPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		case isSlice(snapshot, child.Type):
			compositeCalls, pres, err = buildSlicePrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		case isTaggedUnionType(unit, snapshot, child.Type):
			// A tagged-union operand is checked BEFORE the plain-enum case
			// below, because a union enum also passes isEnumType: its output
			// needs the union's payload recursion, not the plain-enum leaf
			// switch (composite print slice 6).
			compositeCalls, pres, err = buildUnionPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		case isEnumType(unit, snapshot, child.Type):
			compositeCalls, pres, err = buildEnumPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		default:
			compositeCalls, pres, err = buildStructPrintOperand(st, unit, snapshot, fileSet, operandID, child, scope, indent, context, width)
		}
		if err != nil {
			return "", "", err
		}
		preParts = append(preParts, pres...)
		calls = append(calls, compositeCalls...)
	}
	// Every print statement produces exactly one line of output: the trailing
	// newline rides on the last fprintf call's format string as an adjacent
	// `"\n"` literal.
	if len(calls) != 0 {
		calls[len(calls)-1].format += `"\n"`
	}
	lines := make([]string, 0, len(calls))
	for _, call := range calls {
		lines = append(lines, call.text(indent))
	}
	return strings.Join(preParts, "\n"), strings.Join(lines, "\n"), nil
}

// buildStructPrintOperand emits one struct operand of a print statement as its
// sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then per field a label call and a value
// call — `<declared type name>{ <field>: <value>, <field>: <value> }` in the
// struct's DECLARED field order, using the struct's own SOURCE field names
// (resolved from the unit's FieldDeclaration nodes, never the generated C
// pebble_field_<member> names) — with the value of every scalar field
// formatted by the same buildScalarPrintParts a bare scalar print operand
// uses. The label is static C text, so a zero-field struct emits the single
// call `fprintf(stdout, "<name>{ }")`. The struct's declared type name and
// field names are recovered from the unit + file set rather than the symbol
// table so the emission works whether or not Emit was given a symbol result.
func buildStructPrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	valueExpr, err := buildStructPrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
	if err != nil {
		return nil, nil, err
	}
	// Materialize the operand once into a per-operand temp so a
	// struct-returning call operand is evaluated exactly once, then read every
	// field off the temp. The temp name derives from the operand's own
	// unwrapped node ID — the stable identity of this operand, unique across
	// the unit — so a print with several struct operands gets distinct temps.
	tempName := fmt.Sprintf("pebble_print_struct_%d", operandID)
	pres := []string{indent + fmt.Sprintf("%s %s = %s;", structTypeName(child.Type), tempName, valueExpr)}
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// buildTuplePrintOperand emits one tuple operand of a print statement as its
// sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then per element a punctuation call and a
// value call — `(<e0>, <e1>, <e2>)` in tuple element order, every scalar
// element formatted by the same buildScalarPrintParts a bare scalar print
// operand uses. A ONE-ELEMENT tuple prints with a trailing comma, `(5,)`, so
// it is never ambiguous with a parenthesized expression (proposal 17 is
// explicit about this). The tuple element order and element types come from
// the tuple type's own Elements() key, never from the construction site, so
// the printed order is the declared type's element order regardless of how the
// operand was written.
func buildTuplePrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	valueExpr, err := buildTuplePrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
	if err != nil {
		return nil, nil, err
	}
	// Materialize the operand once into a per-operand temp so a tuple-returning
	// call operand is evaluated exactly once, then read every element off the
	// temp as <temp>._<ordinal> (the tuple typedef's positional field names).
	tempName := fmt.Sprintf("pebble_print_tuple_%d", operandID)
	pres := []string{indent + fmt.Sprintf("%s %s = %s;", tupleTypeName(child.Type), tempName, valueExpr)}
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// buildArrayPrintOperand emits one fixed-array operand of a print statement as
// its sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then per element a punctuation call and a
// value call — `[<e0>, <e1>, <e2>]` in array order. The array length is part
// of the type, so the sequence is compile-time unrolled exactly like the
// struct field unrolling; every scalar element is formatted by the same
// buildScalarPrintParts a bare scalar print operand uses. Elements are read
// off the temp as <temp>.data[<i>] (the array typedef wraps a data[N] field).
func buildArrayPrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	valueExpr, err := buildArrayPrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
	if err != nil {
		return nil, nil, err
	}
	// Materialize the operand once into a per-operand temp so an
	// array-returning call operand is evaluated exactly once, then read every
	// element off the temp as <temp>.data[<i>].
	tempName := fmt.Sprintf("pebble_print_array_%d", operandID)
	pres := []string{indent + fmt.Sprintf("%s %s = %s;", arrayTypeName(child.Type), tempName, valueExpr)}
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// buildSlicePrintOperand emits one slice operand of a print statement as its
// sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then the operand's `[` punctuation call,
// a single RAW C for-loop entry iterating the slice's RUNTIME length, and the
// `]` punctuation call. The loop's body is the element formatter generated at
// Go-compile-time for the slice's element type (composite print slice 4: a
// slice's element count is not known until the C program runs, so unlike a
// fixed array it cannot unroll one value call per element — the whole runtime
// loop is one pre-rendered block; see buildSlicePrintValueCalls). The operand
// is materialized once into a per-operand temp so a slice-returning call
// operand is evaluated exactly once, then every element is read off the temp
// as <temp>.data[<i>]. An INLINE slice construction operand (`print arr[:];` —
// the CheckedSlice shape) needs its checked-start temp statement (and any
// backing-array declaration for an array-literal base) emitted as leading pre-
// statements at this statement's indent, so it is handled here rather than in
// buildSlicePrintValueExpr (which has no indent); the construction expression
// becomes the materialized temp's initializer.
func buildSlicePrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	var valueExpr string
	var pres []string
	var err error
	if child.Kind == tir.CheckedSlice {
		tempDecl, constructionExpr, err := buildSliceConstruction(st, unit, snapshot, fileSet, child, scope, indent, context, width, fmt.Sprintf("pebble_print_slice_start_%d", operandID), fmt.Sprintf("pebble_print_slice_backing_%d", operandID))
		if err != nil {
			return nil, nil, err
		}
		valueExpr, pres = constructionExpr, []string{tempDecl}
	} else {
		valueExpr, err = buildSlicePrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
		if err != nil {
			return nil, nil, err
		}
	}
	tempName := fmt.Sprintf("pebble_print_slice_%d", operandID)
	pres = append(pres, indent+fmt.Sprintf("%s %s = %s;", sliceTypeName(child.Type), tempName, valueExpr))
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// buildEnumPrintOperand emits one plain-enum operand of a print statement as
// its sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then the operand's ONE raw C switch over
// the enum's discriminant (composite print slice 5). An enum operand has no
// nested fields to recurse into, so — unlike a struct/tuple/array operand —
// buildPrintValueCalls contributes no per-element value calls; the switch is
// the operand's whole output (see buildEnumPrintValueCalls). The operand is
// materialized once into a per-operand temp (pebble_print_enum_<nodeID>,
// declared with the enum's own pebble_enum_<typeID>_t C type) so an
// enum-returning call operand is evaluated exactly once, and the switch
// compares the temp's stored discriminant against the variant constants.
func buildEnumPrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	valueExpr, err := buildEnumPrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
	if err != nil {
		return nil, nil, err
	}
	// Materialize the operand once into a per-operand temp so an
	// enum-returning call operand is evaluated exactly once, then switch on
	// the temp's stored discriminant. The temp name derives from the
	// operand's own unwrapped node ID — the stable identity of this operand,
	// unique across the unit — so a print with several enum operands gets
	// distinct temps.
	tempName := fmt.Sprintf("pebble_print_enum_%d", operandID)
	pres := []string{indent + fmt.Sprintf("%s %s = %s;", enumTypeName(child.Type), tempName, valueExpr)}
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// buildUnionPrintOperand emits one tagged-union operand of a print statement as
// its sequence of fprintf calls: the materialized-value temp declaration (a
// pre-statement at the same indent), then the operand's ONE raw C switch over
// the union's .tag discriminant (composite print slice 6). The operand is
// materialized once into a per-operand temp (pebble_print_union_<nodeID>,
// declared with the union's own pebble_union_<typeID>_t C type) so a
// union-returning call operand is evaluated exactly once, and the switch
// compares the temp's stored tag against the variant constants (see
// buildUnionPrintValueCalls).
func buildUnionPrintOperand(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	valueExpr, err := buildUnionPrintValueExpr(st, unit, snapshot, fileSet, operandID, child, scope, context, width)
	if err != nil {
		return nil, nil, err
	}
	// Materialize the operand once into a per-operand temp so a
	// union-returning call operand is evaluated exactly once, then switch on
	// the temp's stored tag. The temp name derives from the operand's own
	// unwrapped node ID — the stable identity of this operand, unique across
	// the unit — so a print with several union operands gets distinct temps.
	tempName := fmt.Sprintf("pebble_print_union_%d", operandID)
	pres := []string{indent + fmt.Sprintf("%s %s = %s;", unionTypeName(child.Type), tempName, valueExpr)}
	calls, valuePres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, child.Type, tempName, operandID, "", indent, context, width)
	if err != nil {
		return nil, nil, err
	}
	return calls, append(pres, valuePres...), nil
}

// the fprintf-call sequence that prints ONE value of resolved type valueType
// whose C expression is expr, reading the value directly from expr — a
// materialized operand temp (the common case, from the build*PrintOperand
// wrappers) or a field/element projection off an enclosing value's temp (the
// recursion case, `temp.pebble_field_<member>` / `temp._<ordinal>` /
// `temp.data[<i>]`). A scalar value produces its one buildScalarPrintParts
// call; a struct/tuple/fixed-array value produces its own punctuation + label
// + value sequence with EVERY field/element recursively routed back through
// this same function (composite print slice 3: recursion into already-
// printable nested aggregates), so a nested aggregate field/element emits its
// nested sequence INLINE within the same print statement, never a separate
// print statement. A slice value (composite print slice 4) produces its `[`/
// `]` calls plus ONE raw pre-rendered C for-loop entry whose body is the
// element formatter built against `<expr>.data[i]` — a runtime-determined
// element count cannot be a compile-time list of calls (see
// buildSlicePrintValueCalls). The operand is materialized exactly once by the
// caller, so a side-effecting outer operand (a struct-returning call) is never
// re-evaluated per nesting level; nested reads are plain projections.
// bufferPath tracks the field/element index path from the operand root ("" for
// the operand itself, "0_2" for field 0's element 2) so every char field — at
// any nesting depth — gets a distinct UTF-8 buffer name (a slice element uses
// the path suffix "_i" for its runtime-indexed position, the loop body's
// single buffer being reused across iterations). The pres slice returned is
// not indented; each caller prefixes indent (mirroring how the slice-1/2
// scalar field builders returned unindented pre-statements).
func buildPrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath string, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	if kind, ok := resolvedBuiltin(snapshot, valueType); ok {
		format, arg, parts, err := buildScalarPrintParts(kind, expr, operandID, bufferPath)
		if err != nil {
			return nil, nil, err
		}
		pres := make([]string, 0, len(parts))
		for _, pre := range parts {
			pres = append(pres, indent+pre)
		}
		return []printFprintfCall{{format: format, args: []string{arg}}}, pres, nil
	}
	key, ok := snapshot.Key(valueType)
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s is not in the type snapshot", context, describeType(snapshot, valueType))
	}
	switch key.Kind() {
	case types.Tuple:
		return buildTuplePrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
	case types.Array:
		return buildArrayPrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
	case types.Slice:
		return buildSlicePrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
	default:
		if isTaggedUnionType(unit, snapshot, valueType) {
			// A tagged union is checked BEFORE the plain-enum branch below,
			// because a union enum also passes isEnumType: its output needs
			// the union's payload recursion inside the raw switch, not the
			// plain-enum leaf switch (composite print slice 6).
			return buildUnionPrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
		}
		if isEnumType(unit, snapshot, valueType) {
			return buildEnumPrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
		}
		return buildStructPrintValueCalls(st, unit, snapshot, fileSet, valueType, expr, operandID, bufferPath, indent, context, width)
	}
}

// buildStructPrintValueCalls emits one struct VALUE (a whole operand temp, or
// a nested struct field off an enclosing temp) as its sequence of fprintf
// calls — `<declared type name>{ <field>: <value>, ... }` in the struct's
// DECLARED field order with the struct's own SOURCE field names, then the
// struct's own ` }` closing punctuation. Every field's value is routed through
// the shared buildPrintValueCalls, so a scalar field produces its one
// buildScalarPrintParts call and a nested struct/tuple/array field produces
// its own inline nested sequence. The label is static C text, so a zero-field
// struct emits the single call `fprintf(stdout, "<name>{ }")`. The struct's
// declared type name and field names are recovered from the unit + file set
// rather than the symbol table so the emission works whether or not Emit was
// given a symbol result.
func buildStructPrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	info, err := resolveStructInfo(unit, snapshot, valueType, nil)
	if err != nil {
		return nil, nil, fmt.Errorf("%s print value of type %s: %v", context, structTypeName(valueType), err)
	}
	typeName, err := structSourceName(unit, fileSet, info.decl)
	if err != nil {
		return nil, nil, err
	}
	var calls []printFprintfCall
	var pres []string
	for i, field := range info.fields {
		fieldName, err := fieldSourceName(unit, fileSet, field.member)
		if err != nil {
			return nil, nil, err
		}
		label := ", " + fieldName + ": "
		if i == 0 {
			label = typeName + "{ " + fieldName + ": "
		}
		calls = append(calls, printFprintfCall{format: `"` + label + `"`})
		childPath := bufferPath
		if childPath != "" {
			childPath += "_"
		}
		childPath += strconv.Itoa(i)
		fieldCalls, fieldPres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, field.typ, expr+fmt.Sprintf(".pebble_field_%d", field.member), operandID, childPath, indent, context, width)
		if err != nil {
			return nil, nil, err
		}
		pres = append(pres, fieldPres...)
		calls = append(calls, fieldCalls...)
	}
	if len(info.fields) == 0 {
		calls = append(calls, printFprintfCall{format: `"` + typeName + `{ }"`})
	} else {
		calls = append(calls, printFprintfCall{format: `" }"`})
	}
	return calls, pres, nil
}

// buildTuplePrintValueCalls emits one tuple VALUE (a whole operand temp, or a
// nested tuple field/element off an enclosing temp) as its sequence of fprintf
// calls — `(<e0>, <e1>, <e2>)` in tuple element order, plus the `")"`
// closing punctuation. A ONE-ELEMENT tuple prints with a trailing comma,
// `(5,)`, so it is never ambiguous with a parenthesized expression (proposal
// 17 is explicit about this). Every element's value is routed through the
// shared buildPrintValueCalls, so a scalar element produces its one
// buildScalarPrintParts call and a nested struct/tuple/array element produces
// its own inline nested sequence. The tuple element order and element types
// come from the tuple type's own Elements() key, never from the construction
// site.
func buildTuplePrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	key, ok := snapshot.Key(valueType)
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which is not in the type snapshot", context, tupleTypeName(valueType))
	}
	elements, ok := key.Elements()
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which has no element list", context, tupleTypeName(valueType))
	}
	var calls []printFprintfCall
	var pres []string
	for i, element := range elements {
		label := ", "
		if i == 0 {
			label = "("
		}
		calls = append(calls, printFprintfCall{format: `"` + label + `"`})
		childPath := bufferPath
		if childPath != "" {
			childPath += "_"
		}
		childPath += strconv.Itoa(i)
		elementCalls, elementPres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, element, expr+fmt.Sprintf("._%d", i), operandID, childPath, indent, context, width)
		if err != nil {
			return nil, nil, err
		}
		pres = append(pres, elementPres...)
		calls = append(calls, elementCalls...)
	}
	if len(elements) == 1 {
		// `(5,)` — the one-element tuple's trailing comma, so the output is
		// unambiguous with a parenthesized expression.
		calls = append(calls, printFprintfCall{format: `","`})
	}
	calls = append(calls, printFprintfCall{format: `")"`})
	return calls, pres, nil
}

// buildArrayPrintValueCalls emits one fixed-array VALUE (a whole operand temp,
// or a nested array field/element off an enclosing temp) as its sequence of
// fprintf calls — `[<e0>, <e1>, <e2>]` in array order, plus the `"]"`
// closing punctuation. The array length is part of the type, so the sequence
// is compile-time unrolled exactly like the struct field unrolling. Every
// element's value is routed through the shared buildPrintValueCalls, so a
// scalar element produces its one buildScalarPrintParts call and a nested
// struct/tuple/array element produces its own inline nested sequence.
// Elements are read off the value expression as <expr>.data[<i>] (the array
// typedef wraps a data[N] field).
func buildArrayPrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	key, ok := snapshot.Key(valueType)
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which is not in the type snapshot", context, describeType(snapshot, valueType))
	}
	length, element, ok := key.Array()
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which has no length and element type", context, describeType(snapshot, valueType))
	}
	var calls []printFprintfCall
	var pres []string
	calls = append(calls, printFprintfCall{format: `"["`})
	for i := uint64(0); i < length; i++ {
		if i != 0 {
			calls = append(calls, printFprintfCall{format: `", "`})
		}
		childPath := bufferPath
		if childPath != "" {
			childPath += "_"
		}
		childPath += strconv.FormatUint(i, 10)
		elementCalls, elementPres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, element, expr+fmt.Sprintf(".data[%d]", i), operandID, childPath, indent, context, width)
		if err != nil {
			return nil, nil, err
		}
		pres = append(pres, elementPres...)
		calls = append(calls, elementCalls...)
	}
	calls = append(calls, printFprintfCall{format: `"]"`})
	return calls, pres, nil
}

// buildSlicePrintValueCalls emits one slice VALUE (a whole operand temp, or a
// nested slice field/element off an enclosing temp) as its sequence of fprintf
// calls — `[` punctuation, ONE raw pre-rendered C for-loop over the slice's
// RUNTIME length, `]` punctuation. Unlike a fixed array, a slice's element
// count is not known when THIS Go code runs (it is known only when the compiled
// C program executes), so the sequence cannot be a compile-time-unrolled list
// of one value call per element; instead the whole loop is built as one
// pre-indented raw statement block (a printFprintfCall whose raw field is set)
// whose body is the element formatter generated at Go-compile-time for the
// element TYPE against the C expression <expr>.data[<i>], so the ONE emitted
// loop body executes N times at C runtime:
//
//	fprintf(stdout, "[");
//	for (size_t <i> = 0; <i> < <expr>.len; <i>++) {
//	    if (<i> != 0) fprintf(stdout, ", ");
//	    /* recursively emitted element formatter for <expr>.data[<i>] */
//	}
//	fprintf(stdout, "]");
//
// Every element's value is routed through the shared buildPrintValueCalls, so
// a scalar element produces its one buildScalarPrintParts call and a nested
// struct/tuple/array element produces its own inline nested sequence — the
// recursion that slices 1-3 established is reused unchanged for the element
// TYPE, only the iteration count is dynamic (proposal 17's own sketch). The
// element formatter's pre-statements (a char element's UTF-8 buffer) land
// INSIDE the loop body, re-declared and re-filled each iteration, so the one
// buffer name (suffix "_i" in bufferPath, reused across iterations) never
// collides with a sibling aggregate's buffers. The loop variable's name also
// carries the bufferPath suffix, so a slice nested inside another slice's
// element gets a distinct loop variable from its parent's. Elements are read
// off the value expression as <expr>.data[<i>] — the slice typedef's own
// data/len field naming (matching buildSliceIndexValue's read convention, with
// the loop's index always in-bounds, so no checked-index helper wraps it).
// No pres are returned: the element formatter's pres are consumed into the
// loop body text rather than hoisted (they must run once per iteration).
func buildSlicePrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	key, ok := snapshot.Key(valueType)
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which is not in the type snapshot", context, describeType(snapshot, valueType))
	}
	element, ok := key.Child()
	if !ok {
		return nil, nil, fmt.Errorf("%s print value of type %s, which has no element type", context, describeType(snapshot, valueType))
	}
	loopVar := fmt.Sprintf("pebble_print_i_%d", operandID)
	childPath := bufferPath
	if childPath != "" {
		childPath += "_"
	}
	childPath += "i"
	if bufferPath != "" {
		loopVar += "_" + bufferPath
	}
	// The element formatter is generated against the element TYPE with the
	// loop's runtime index in the element's C expression, exactly as a static
	// field/element projection is, and at the loop body's own indent so every
	// pre-statement and call it produces lines up inside the loop.
	loopIndent := indent + "    "
	elementCalls, elementPres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, element, expr+fmt.Sprintf(".data[%s]", loopVar), operandID, childPath, loopIndent, context, width)
	if err != nil {
		return nil, nil, err
	}
	var body strings.Builder
	body.WriteString(loopIndent + "if (" + loopVar + " != 0) fprintf(stdout, \", \");\n")
	for _, pre := range elementPres {
		body.WriteString(pre + "\n")
	}
	for _, call := range elementCalls {
		body.WriteString(call.text(loopIndent) + "\n")
	}
	var block strings.Builder
	block.WriteString(indent + "for (size_t " + loopVar + " = 0; " + loopVar + " < " + expr + ".len; " + loopVar + "++) {\n")
	block.WriteString(body.String())
	block.WriteString(indent + "}")
	return []printFprintfCall{
		{format: `"["`},
		{raw: block.String()},
		{format: `"]"`},
	}, nil, nil
}

// buildEnumPrintValueCalls emits one plain-enum VALUE (a whole operand temp,
// or a nested enum field/element off an enclosing temp) as its ONE raw
// pre-rendered C switch over the enum's discriminant (composite print slice 5).
// A variant's output is a STATIC string — the declared type name, a literal
// `.`, and the matching variant's declared SOURCE name — selected by a RUNTIME
// tag comparison, so it can never fold into the combined printf the way a
// scalar can and it has no nested fields to unroll into per-element calls;
// instead the whole switch is one printFprintfCall whose raw field is set:
//
//	switch (<expr>) {
//	    case pebble_variant_<m1>:
//	        fprintf(stdout, "Color.red");
//	        break;
//	    case pebble_variant_<m2>:
//	        fprintf(stdout, "Color.green");
//	        break;
//	    default:
//	        fprintf(stdout, "Color<invalid: %d>", <expr>);
//	        break;
//	}
//
// The case labels are the variant constants of the enum's own C typedef
// (pebble_variant_<member>, the exact constants buildCaseLabel reuses for an
// enum subject's switch — this is a NEW consumer of the existing variant-to-C-
// constant mapping, not new enum representation knowledge), and the switch's
// subject is the enum value's C expression (a materialized temp, or a nested
// field/element projection like <temp>.pebble_field_<member>), whose C type is
// the enum typedef itself — integral, so the C switch compares it directly.
// The default case is the proposal-17 defensive invalid-discriminant output
// `Color<invalid: <discriminant>>`, genuinely unreachable for a well-formed
// program but present so a memory-corruption scenario never reads a garbage
// value as a valid name. A trailing empty-string label call follows the raw
// switch: it is a no-op (`fprintf(stdout, "");`) in the middle of a multi-
// operand statement, and buildSequentialPrint's trailing-newline append turns
// it into `fprintf(stdout, "\n");` when the enum is the statement's last
// operand — the raw switch block itself cannot receive that append, so the
// no-op carrier is how the one-newline-per-print rule stays uniform. The
// variant-name and type-name strings come from the unit's own declaration-node
// spans (variantSourceName/enumSourceName), never the generated C names. No
// pres are returned — the enum has no pre-statement-bearing scalar children.
func buildEnumPrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	info, err := resolveEnumInfo(unit, snapshot, valueType)
	if err != nil {
		return nil, nil, fmt.Errorf("%s print value of type %s: %v", context, enumTypeName(valueType), err)
	}
	typeName, err := enumSourceName(unit, fileSet, info.decl)
	if err != nil {
		return nil, nil, err
	}
	caseIndent := indent + "    "
	bodyIndent := indent + "        "
	var block strings.Builder
	block.WriteString(indent + "switch (" + expr + ") {\n")
	for _, variant := range info.variants {
		variantName, err := variantSourceName(unit, fileSet, variant)
		if err != nil {
			return nil, nil, err
		}
		block.WriteString(caseIndent + "case " + enumVariantName(variant) + ":\n")
		block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(typeName+"."+variantName) + ");\n")
		block.WriteString(bodyIndent + "break;\n")
	}
	block.WriteString(caseIndent + "default:\n")
	block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(typeName+"<invalid: %d>") + ", " + expr + ");\n")
	block.WriteString(bodyIndent + "break;\n")
	block.WriteString(indent + "}")
	return []printFprintfCall{
		{raw: block.String()},
		{format: `""`},
	}, nil, nil
}

// buildUnionPrintValueCalls emits one tagged-union VALUE (a whole operand temp,
// or a nested union field/element off an enclosing temp) as its ONE raw
// pre-rendered C switch over the union's .tag discriminant (composite print
// slice 6). A variant's STATIC prefix — the declared type name, a literal `.`,
// and the matching variant's declared SOURCE name — is selected by a RUNTIME
// tag comparison, and a payload-carrying variant ALSO recurses into its payload
// (read as <expr>.payload.pebble_field_<member>), so the output can never fold
// into the combined printf the way a scalar can; instead the whole switch is
// one printFprintfCall whose raw field is set:
//
//	switch (<expr>.tag) {
//	    case pebble_variant_<m1>:
//	        fprintf(stdout, "Result.ok(");
//	        fprintf(stdout, "%" PRId32, <expr>.payload.pebble_field_<m1>);
//	        fprintf(stdout, ")");
//	        break;
//	    case pebble_variant_<m2>:
//	        fprintf(stdout, "Result.done");
//	        break;
//	    default:
//	        fprintf(stdout, "Result<invalid-tag: %d>", <expr>.tag);
//	        break;
//	}
//
// The case labels are the variant constants of the union's own C tag-enum
// typedef (pebble_variant_<member>, the exact constants every union switch
// subject uses — buildUnionConstruction sets the .tag field to them, and the
// tagged-union switch subject reads them), so the stored tag and the case
// labels agree by construction. The switch's subject is the union value's C
// expression (a materialized temp, or a nested field/element projection like
// <temp>.pebble_field_<member>) with `.tag` appended — the union typedef's own
// field naming (see buildUnionTypedef). A payload-carrying variant is one whose
// payload member the union's C typedef actually declares — exactly
// unionVariantPayloadMember, the same read-side test the narrowed union-variant
// payload access uses (a variant declared void, or declared with a payload but
// never constructed anywhere, has no union member and prints bare). Its payload
// value is routed through the shared buildPrintValueCalls against the payload
// projection, so the payload can itself be any currently-printable type — a
// scalar, a struct, a tuple, an array, a slice, an enum, or another union — and
// the payload's pre-statements (a char payload's UTF-8 buffer) land INSIDE the
// case body at the case's own indent, declared and filled only when that case
// runs. The payload projection is read off the SAME value expression the switch
// tests (never a re-evaluated operand), so a union-returning call operand
// materialized once by buildUnionPrintOperand is not re-evaluated per variant.
// The default case is the proposal-17 defensive invalid-tag output
// `Type<invalid-tag: <tag>>`, genuinely unreachable for a well-formed program
// but present so a memory-corruption scenario never reads a garbage tag as a
// valid variant name. A trailing empty-string label call follows the raw
// switch: it is a no-op (`fprintf(stdout, "");`) in the middle of a multi-
// operand statement, and buildSequentialPrint's trailing-newline append turns
// it into `fprintf(stdout, "\n");` when the union is the statement's last
// operand — the raw switch block itself cannot receive that append, so the
// no-op carrier is how the one-newline-per-print rule stays uniform. The
// variant-name and type-name strings come from the unit's own declaration-node
// spans (variantSourceName/unionSourceName), never the generated C names. No
// pres are returned — the payload formatter's pres are consumed into the case
// body text rather than hoisted (they must run only when that case executes).
func buildUnionPrintValueCalls(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, valueType types.TypeID, expr string, operandID tir.NodeID, bufferPath, indent, context string, width types.BuiltinKind) ([]printFprintfCall, []string, error) {
	info, err := resolveUnionInfoForValue(unit, snapshot, valueType)
	if err != nil {
		return nil, nil, fmt.Errorf("%s print value of type %s: %v", context, unionTypeName(valueType), err)
	}
	typeName, err := unionSourceName(unit, fileSet, info.decl)
	if err != nil {
		return nil, nil, err
	}
	caseIndent := indent + "    "
	bodyIndent := indent + "        "
	var block strings.Builder
	block.WriteString(indent + "switch (" + expr + ".tag) {\n")
	for _, variant := range info.variants {
		variantName, err := variantSourceName(unit, fileSet, variant)
		if err != nil {
			return nil, nil, err
		}
		block.WriteString(caseIndent + "case " + enumVariantName(variant) + ":\n")
		if unionVariantPayloadMember(unit, snapshot, valueType, variant) {
			payloadType, ok := unionMemberType(info.members, variant)
			if !ok {
				return nil, nil, fmt.Errorf("%s print value of type %s: variant symbol %d has a payload union member but no resolved payload type", context, unionTypeName(valueType), variant)
			}
			block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(typeName+"."+variantName+"(") + ");\n")
			payloadCalls, payloadPres, err := buildPrintValueCalls(st, unit, snapshot, fileSet, payloadType, expr+fmt.Sprintf(".payload.pebble_field_%d", variant), operandID, bufferPath, bodyIndent, context, width)
			if err != nil {
				return nil, nil, err
			}
			for _, pre := range payloadPres {
				block.WriteString(pre + "\n")
			}
			for _, call := range payloadCalls {
				block.WriteString(call.text(bodyIndent) + "\n")
			}
			block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(")") + ");\n")
		} else {
			block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(typeName+"."+variantName) + ");\n")
		}
		block.WriteString(bodyIndent + "break;\n")
	}
	block.WriteString(caseIndent + "default:\n")
	block.WriteString(bodyIndent + "fprintf(stdout, " + strconv.Quote(typeName+"<invalid-tag: %d>") + ", " + expr + ".tag);\n")
	block.WriteString(bodyIndent + "break;\n")
	block.WriteString(indent + "}")
	return []printFprintfCall{
		{raw: block.String()},
		{format: `""`},
	}, nil, nil
}

// buildTuplePrintValueExpr builds the C expression naming one tuple-typed print
// operand's value, of the shapes real source produces (all built by the same
// machinery a tuple-typed call argument uses): a reference to a tuple-typed
// local (a SymbolValue, emitted as its pebble_local_<id> C name — the tuple
// typedef makes a whole by-value copy trivially valid C), a whole-tuple read (a
// Load of a tuple-typed place, emitted as the place's lvalue), a freshly
// constructed tuple literal (a TupleValue, emitted as its C99 compound literal
// by buildTupleValueExpr), or a call to a tuple-returning helper (a DirectCall,
// emitted as the call expression). Any other shape is a clean rejection, never a
// guessed lowering.
func buildTuplePrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	switch child.Kind {
	case tir.SymbolValue:
		info, ok := scope[child.Symbol]
		if !ok || info.tuple != child.Type {
			return "", fmt.Errorf("%s print operand references symbol %d, which is not a tuple-typed local of type %s in scope", context, child.Symbol, tupleTypeName(child.Type))
		}
		return fmt.Sprintf("pebble_local_%d", child.Symbol), nil
	case tir.Load:
		if len(child.Children) != 1 {
			return "", fmt.Errorf("%s print operand is a Load with %d child(ren), want exactly one place", context, len(child.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isTuple(snapshot, placeType) {
			return "", fmt.Errorf("%s print operand is a Load of a place of type %s, want a tuple-typed place", context, describeType(snapshot, placeType))
		}
		return lvalue, nil
	case tir.TupleValue:
		// A print operand has no declared target type of its own — the
		// operand's type IS the tuple being printed — so the literal's own
		// type is deliberately passed through unchanged as the cast's target
		// (the same value a tuple-typed local's declaration records).
		return buildTupleValueExpr(st, unit, snapshot, fileSet, child, scope, child.Type, context, width)
	case tir.DirectCall:
		return buildDirectCall(st, unit, snapshot, fileSet, child, scope, width)
	}
	return "", fmt.Errorf("%s print operand is a %s of tuple type %s, which this backend does not lower as a print operand", context, child.Kind, describeType(snapshot, child.Type))
}

// buildArrayPrintValueExpr builds the C expression naming one fixed-array
// print operand's value, of the shapes real source produces (all built by the
// same machinery an array-typed call argument uses): a reference to an
// array-typed local (a SymbolValue, emitted as its pebble_local_<id> C name
// when the local is a pebble_array_<typeID>_t wrapped value — the call-
// initialized shape — or as the array typedef's compound literal wrapping the
// raw C array's elements element-by-element for a plain `[1, 2, 3]`-initialized
// local, exactly as buildArrayArgument does), a freshly constructed array
// literal (an ArrayValue, emitted as its C99 compound literal via the shared
// buildArrayBraceElements element builder), or a call to an array-returning
// helper (a DirectCall, emitted as the call expression). Any other shape is a
// clean rejection, never a guessed lowering.
func buildArrayPrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(child.Type)
	if !ok {
		return "", fmt.Errorf("%s print operand is an array of type %s, which is not in the type snapshot", context, describeType(snapshot, child.Type))
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("%s print operand is an array of type %s, which has no length and element type", context, describeType(snapshot, child.Type))
	}
	switch child.Kind {
	case tir.SymbolValue:
		info, ok := scope[child.Symbol]
		if !ok || info.array != child.Type {
			return "", fmt.Errorf("%s print operand references symbol %d, which is not an array-typed local of type %s in scope", context, child.Symbol, describeType(snapshot, child.Type))
		}
		if info.arrayWrapped {
			return fmt.Sprintf("pebble_local_%d", child.Symbol), nil
		}
		values := make([]string, 0, int(length))
		for i := uint64(0); i < length; i++ {
			values = append(values, fmt.Sprintf("pebble_local_%d[%d]", child.Symbol, i))
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(child.Type), strings.Join(values, ", ")), nil
	case tir.Load:
		if len(child.Children) != 1 {
			return "", fmt.Errorf("%s print operand is a Load with %d child(ren), want exactly one place", context, len(child.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isArray(snapshot, placeType) {
			return "", fmt.Errorf("%s print operand is a Load of a place of type %s, want an array-typed place", context, describeType(snapshot, placeType))
		}
		values := make([]string, 0, int(length))
		for i := uint64(0); i < length; i++ {
			values = append(values, fmt.Sprintf("%s[%d]", lvalue, i))
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(child.Type), strings.Join(values, ", ")), nil
	case tir.ArrayValue:
		elementExprs, err := buildArrayBraceElements(st, unit, snapshot, fileSet, child, scope, context, width, element)
		if err != nil {
			return "", err
		}
		if uint64(len(elementExprs)) != length {
			return "", fmt.Errorf("%s print operand is an array of type %s with %d element expression(s), want %d", context, describeType(snapshot, child.Type), len(elementExprs), length)
		}
		return fmt.Sprintf("(%s){ .data = { %s } }", arrayTypeName(child.Type), strings.Join(elementExprs, ", ")), nil
	case tir.DirectCall:
		return buildDirectCall(st, unit, snapshot, fileSet, child, scope, width)
	}
	return "", fmt.Errorf("%s print operand is a %s of array type %s, which this backend does not lower as a print operand", context, child.Kind, describeType(snapshot, child.Type))
}

// buildSlicePrintValueExpr builds the C expression naming one slice-typed print
// operand's value, of the shapes real source produces (all built by the same
// machinery a slice-typed call argument or slice local declaration uses): a
// reference to a slice-typed local (a SymbolValue, emitted as its
// pebble_local_<id> C name), a whole-slice read (a Load of a slice-typed
// place, emitted as the place's lvalue), a call to a slice-returning helper (a
// DirectCall/MethodCall, emitted as the call expression), or a raw-pointer-
// backed slice (a SliceFromRaw, emitted as its construction expression). A
// fresh slice construction (a CheckedSlice — `print arr[:];`) is handled by
// the caller instead, buildSlicePrintOperand, because its checked-start temp
// statement needs the statement indent this builder does not receive. Any
// other shape is a clean rejection, never a guessed lowering.
func buildSlicePrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	switch child.Kind {
	case tir.SymbolValue:
		info, ok := scope[child.Symbol]
		if !ok || info.sliceType != child.Type {
			return "", fmt.Errorf("%s print operand references symbol %d, which is not a slice-typed local of type %s in scope", context, child.Symbol, sliceTypeName(child.Type))
		}
		return fmt.Sprintf("pebble_local_%d", child.Symbol), nil
	case tir.Load:
		if len(child.Children) != 1 {
			return "", fmt.Errorf("%s print operand is a Load with %d child(ren), want exactly one place", context, len(child.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isSlice(snapshot, placeType) {
			return "", fmt.Errorf("%s print operand is a Load of a place of type %s, want a slice-typed place", context, describeType(snapshot, placeType))
		}
		return lvalue, nil
	case tir.DirectCall, tir.MethodCall:
		return buildDirectCall(st, unit, snapshot, fileSet, child, scope, width)
	case tir.SliceFromRaw:
		return buildRawSliceConstruction(st, unit, snapshot, fileSet, child, scope, width, context)
	}
	return "", fmt.Errorf("%s print operand is a %s of slice type %s, which this backend does not lower as a print operand", context, child.Kind, describeType(snapshot, child.Type))
}

// buildEnumPrintValueExpr builds the C expression naming one enum-typed print
// operand's value, of the shapes real source produces (all built by the same
// machinery an enum value uses anywhere in this backend — buildEnumValue is
// the one shared builder for an enum value in every position): a variant
// literal (Color.green, emitted as its pebble_variant_<member> C constant), a
// reference to an enum-typed local/global/extern (a SymbolValue, emitted as
// its pebble_local_<id> / pebble_global_<id> / extern C name), a call to an
// enum-returning helper (a DirectCall, emitted as the call expression), a
// SourceAlias (transparent grouped-expression parens), a Load of an enum-typed
// struct field (emitted as the field projection), an integer-to-enum cast, or
// an enum-payload optional force-unwrap. Any other shape is a clean rejection,
// never a guessed lowering. This is a NEW consumer of the existing buildEnumValue
// machinery (composite print slice 5), not new enum representation knowledge.
func buildEnumPrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	return buildEnumValue(st, unit, snapshot, fileSet, operandID, scope, width)
}

// buildUnionPrintValueExpr builds the C expression naming one tagged-union
// print operand's value, of the shapes real source produces (all built by the
// same machinery a union value uses anywhere in this backend — buildUnionValueExpr
// is the one shared builder for a union value in every position): a variant
// construction (Result.ok(42), emitted as its union compound literal), a
// reference to a union-typed local/global/extern (a SymbolValue, emitted as its
// pebble_local_<id> / pebble_global_<id> / extern C name), a call to a
// union-returning helper (a DirectCall, emitted as the call expression), a
// SourceAlias (transparent grouped-expression parens), a Load of a union-typed
// struct field (emitted as the field projection), or a union-payload optional
// force-unwrap. Any other shape is a clean rejection, never a guessed
// lowering. This is a NEW consumer of the existing buildUnionValueExpr machinery
// (composite print slice 6), not new union representation knowledge.
func buildUnionPrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	return buildUnionValueExpr(st, unit, snapshot, fileSet, operandID, scope, context, child.Type, width)
}

// buildStructPrintValueExpr builds the C expression naming one struct-typed
// print operand's value, of the four shapes real source produces (all built by
// the same machinery a struct-typed call argument uses): a reference to a
// struct-typed local/global (a SymbolValue, emitted as its pebble_local_<id> /
// pebble_global_<id> C name), a whole-struct read (a Load of a struct-typed
// place, emitted as the place's lvalue), a freshly-constructed struct literal
// (a RecordConstruct, emitted as its C99 compound literal), or a call to a
// struct-returning helper (a DirectCall, emitted as the call expression). Any
// other shape is a clean rejection, never a guessed lowering.
func buildStructPrintValueExpr(st *emitState, unit *tir.Unit, snapshot *types.Snapshot, fileSet *source.FileSet, operandID tir.NodeID, child tir.Node, scope map[symbol.SymbolID]localInfo, context string, width types.BuiltinKind) (string, error) {
	switch child.Kind {
	case tir.SymbolValue:
		if name, ok := localOrGlobalName(st, child.Symbol, scope); ok {
			return name, nil
		}
		return "", fmt.Errorf("%s print operand references symbol %d, which is not a struct-typed local or global in scope", context, child.Symbol)
	case tir.Load:
		if len(child.Children) != 1 {
			return "", fmt.Errorf("%s print operand is a Load with %d child(ren), want exactly one place", context, len(child.Children))
		}
		lvalue, placeType, err := buildPlaceLValue(st, unit, snapshot, fileSet, child.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		if !isStruct(snapshot, placeType) || isEnumType(unit, snapshot, placeType) {
			return "", fmt.Errorf("%s print operand is a Load of a place of type %s, want a struct-typed place", context, describeType(snapshot, placeType))
		}
		return lvalue, nil
	case tir.RecordConstruct:
		return buildStructValueExpr(st, unit, snapshot, fileSet, child, scope, context, width)
	case tir.DirectCall:
		return buildDirectCall(st, unit, snapshot, fileSet, child, scope, width)
	}
	return "", fmt.Errorf("%s print operand is a %s of struct type %s, which this backend does not lower as a print operand", context, child.Kind, describeType(snapshot, child.Type))
}

// sourceNameAt slices the identifier text covering one node's span out of its
// source file. It is how the print path recovers the DECLARED names (the
// struct type name and its field names) that must appear in the output, and it
// is deliberately independent of the symbol table: Emit may be given a nil
// symbol result (the common test-harness shape), so the names come from the
// unit's own declaration-node spans instead.
func sourceNameAt(fileSet *source.FileSet, span source.Span) (string, error) {
	if fileSet == nil {
		return "", fmt.Errorf("no source file set is available to recover a declared name from its span")
	}
	file, ok := fileSet.File(span.Source)
	if !ok {
		return "", fmt.Errorf("declared-name span names a source file that is not in the file set")
	}
	name := string(file.Slice(span))
	if name == "" {
		return "", fmt.Errorf("declared-name span covers no source text")
	}
	return name, nil
}

// structSourceName resolves one struct type's declared source name (Point) by
// slicing the type's own TypeDeclaration node span out of its source file.
func structSourceName(unit *tir.Unit, fileSet *source.FileSet, decl symbol.SymbolID) (string, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.TypeDeclaration && node.Symbol == decl {
			return sourceNameAt(fileSet, node.Span)
		}
	}
	return "", fmt.Errorf("struct declaration symbol %d has no TypeDeclaration node in the unit", decl)
}

// fieldSourceName resolves one struct field's declared source name (x) by
// slicing the field's own FieldDeclaration node span out of its source file.
func fieldSourceName(unit *tir.Unit, fileSet *source.FileSet, member symbol.SymbolID) (string, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FieldDeclaration && node.Symbol == member {
			return sourceNameAt(fileSet, node.Span)
		}
	}
	return "", fmt.Errorf("struct field symbol %d has no FieldDeclaration node in the unit", member)
}

// enumSourceName resolves one enum type's declared source name (Color) by
// slicing the type's own TypeDeclaration node span out of its source file —
// the same span mechanism structSourceName uses for a struct's declared name,
// shared because a TypeDeclaration node carries the declared identifier for
// every nominal kind.
func enumSourceName(unit *tir.Unit, fileSet *source.FileSet, decl symbol.SymbolID) (string, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.TypeDeclaration && node.Symbol == decl {
			return sourceNameAt(fileSet, node.Span)
		}
	}
	return "", fmt.Errorf("enum declaration symbol %d has no TypeDeclaration node in the unit", decl)
}

// unionSourceName resolves one tagged-union type's declared source name
// (Result) by slicing the type's own TypeDeclaration node span out of its
// source file — the same span mechanism enumSourceName/structSourceName use
// for an enum's/struct's declared name, shared because a TypeDeclaration node
// carries the declared identifier for every nominal kind.
func unionSourceName(unit *tir.Unit, fileSet *source.FileSet, decl symbol.SymbolID) (string, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.TypeDeclaration && node.Symbol == decl {
			return sourceNameAt(fileSet, node.Span)
		}
	}
	return "", fmt.Errorf("union declaration symbol %d has no TypeDeclaration node in the unit", decl)
}

// variantSourceName resolves one enum variant's declared source name (red) by
// slicing the variant's own VariantDeclaration node span out of its source
// file — the same span mechanism fieldSourceName uses for a struct field's
// declared name (the checker emits one VariantDeclaration node per enum
// member, with the member symbol's span covering the variant's identifier).
func variantSourceName(unit *tir.Unit, fileSet *source.FileSet, member symbol.SymbolID) (string, error) {
	for _, node := range unit.Nodes() {
		if node.Kind == tir.VariantDeclaration && node.Symbol == member {
			return sourceNameAt(fileSet, node.Span)
		}
	}
	return "", fmt.Errorf("enum variant symbol %d has no VariantDeclaration node in the unit", member)
}
