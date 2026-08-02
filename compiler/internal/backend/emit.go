// Package backend lowers typed IR to C source emitted against the versioned
// runtime ABI (runtime/include/pebble_rt.h). It is deliberately narrow: the
// current slice emits exactly two entry shapes — an empty-bodied Pebble-
// convention void entry function, and a zero-parameter integer entry whose
// width (i32 or i64) is decided once by the entry's own result type and never
// mixed within a body. The body matches a single recursive block grammar: a
// block is zero or more `let <name> <width> = <expression>;` /
// `var <name> <width> = <expression>;` local declarations, plus
// `x = <expression>;` reassignments of an already-declared local, and a
// `while <condition> { <loop body> }` loop statement, followed by a tail that
// is either one `return <expression>;` or a two-armed
// `if <condition> { <block> } else { <block> }`; a condition is an integer
// comparison, a ==/!= equality between two bool values, a bare bool value, or
// a && / || combination of those, built by buildCondition, and the two arms
// are themselves blocks under the same rule, so an
// arm may contain its own locals, reassignments, nested if/else, and loops. A
// while loop's body is a block of local declarations, reassignments, if
// statements (a loop-body if is built by buildLoopIf and has an optional
// else), nested while loops (built by buildWhile), and break/continue
// statements (built by buildLoopJump), with no required tail (see
// buildLoopBody); a while can only be a leading statement, never the block's
// tail. Locals declared in an enclosing block are visible in a nested block;
// locals declared inside an arm or loop body are visible only within that
// scope. Every expression in an accepted body must carry the entry's own
// width — a local of the other width (an i32 local inside an i64 entry, or
// vice versa) is a clean width-mismatch rejection, never a coercion, since
// this backend has no cast/coercion lowering yet. Everything else is rejected
// with a descriptive error instead of guessed.
package backend

import (
	"fmt"
	"io"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/syntax"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// Emit writes C source for unit's designated entry function to w. The entry
// function (identified by entrySymbol) must be Pebble-convention and take zero
// parameters. Its result must be either void with a completely empty body (no
// statements — only ever an ImplicitReturn, i.e. exactly what `fn main() void
// {}` produces) or i32/i64 with a body matching the recursive block grammar: a
// block is zero or more `let <name> <width> = <expression>;` /
// `var <name> <width> = <expression>;` local declarations, plus
// `x = <expression>;` reassignments of an already-declared local (a tir.Store;
// see buildBlock) and `while <comparison> { <loop body> }` loop statements (a
// tir.While; see buildWhile), followed by a tail that is either one
// `return <expression>;` or a two-armed `if <condition> { <block> } else {
// <block> }` whose condition is an integer comparison (<, <=, >, >=, ==, !=), a
// ==/!= equality between two bool values, a bare bool value, or a && / ||
// combination of those (see buildCondition);
// each arm is itself a block under the same grammar, so an arm may contain its
// own locals and nested if/else. Every expression — a local's initializer, a
// reassignment's new value, a return value, or an if/else arm's return value —
// may be a plain non-negative integer literal, a tree of checked negation and
// checked +, -, *, /, % arithmetic (see buildExpr), or a reference to a local
// declared earlier in the same or an enclosing block. A comparison's operands
// are additionally allowed to be int-typed integer literals (see
// buildComparisonOperand), or — for ==/!= — two bool values built under the
// bool grammar (see buildComparison). Checked operations emit
// pebble_rt_checked_*_i32 /
// pebble_rt_checked_*_i64 calls, chosen by the entry's resolved width, so the
// language's overflow and divide-by-zero semantics survive into the emitted
// program; comparisons emit the plain C operator, which cannot overflow. The
// entry's width — i32 or i64, from its own result type — is resolved once here
// and threaded through every builder below; a body that mixes widths is
// rejected, never coerced. Any other shape returns a descriptive error and
// writes nothing to w; this package does not yet lower arbitrary expressions
// or statements.
func Emit(unit *tir.Unit, snapshot *types.Snapshot, entrySymbol symbol.SymbolID, w io.Writer) error {
	if unit == nil {
		return fmt.Errorf("cannot emit C: nil typed-IR unit")
	}
	if snapshot == nil {
		return fmt.Errorf("cannot emit C: nil type snapshot")
	}
	if w == nil {
		return fmt.Errorf("cannot emit C: nil writer")
	}

	decl, err := findEntryDeclaration(unit, entrySymbol)
	if err != nil {
		return err
	}
	result, err := validateEntrySignature(decl, snapshot)
	if err != nil {
		return err
	}
	block, blockID, err := findEntryBody(unit, decl)
	if err != nil {
		return err
	}
	if result == types.Void {
		if err := validateEmptyBody(unit, block); err != nil {
			return err
		}
		return emitEntryC(w, voidEntryUserMain, voidEntryMainBody)
	}
	statements, err := buildBlock(unit, snapshot, blockID, nil, 0, result)
	if err != nil {
		return err
	}
	return emitEntryC(w, fmt.Sprintf(integerEntryUserMain, entryReturnType(result), statements), integerEntryMainBody)
}

// findEntryDeclaration locates the FunctionDeclaration node for entrySymbol.
// A specialization would carry non-empty TypeArgs; the entry cannot be
// generic, so those are deliberately excluded rather than assumed absent.
func findEntryDeclaration(unit *tir.Unit, entrySymbol symbol.SymbolID) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.FunctionDeclaration || node.Symbol != entrySymbol || len(node.TypeArgs) != 0 {
			continue
		}
		return node, nil
	}
	return tir.Node{}, fmt.Errorf("entry function not found in unit: no non-generic FunctionDeclaration for symbol %d", entrySymbol)
}

// validateEntrySignature checks the entry's calling convention, parameter
// count, and result type against the supported shapes: a void result (empty
// body) or an i32/i64 result (body under the recursive block grammar). On
// success it returns the resolved result builtin (types.Void, types.I32, or
// types.I64) — for an integer entry that returned builtin IS the width every
// builder downstream emits at, threaded through Emit rather than re-derived.
// Whether the body actually matches the result's shape is decided by the
// body-validation step the caller dispatches on.
func validateEntrySignature(decl tir.Node, snapshot *types.Snapshot) (types.BuiltinKind, error) {
	if decl.Convention != types.Pebble {
		return 0, fmt.Errorf("entry function uses %s calling convention, want Pebble", callingConventionName(decl.Convention))
	}
	if len(decl.Parameters) != 0 {
		return 0, fmt.Errorf("entry function has %d parameter(s), want 0 (main([]str) and main(i32, []str) are not supported yet)", len(decl.Parameters))
	}
	key, ok := snapshot.Key(decl.ResultType)
	if !ok {
		return 0, fmt.Errorf("entry function result type %d is not in the type snapshot", decl.ResultType)
	}
	builtin, ok := key.Builtin()
	if !ok || (builtin != types.Void && builtin != types.I32 && builtin != types.I64) {
		return 0, fmt.Errorf("entry function result type is %s, want void, i32, or i64", describeType(snapshot, decl.ResultType))
	}
	return builtin, nil
}

// findEntryBody follows the entry declaration's FunctionID to its FunctionDecl
// and resolves that declaration's body node. The body node is a distinct
// Block entry in unit.Nodes(), separate from the FunctionDeclaration node
// found by findEntryDeclaration. It returns both the resolved Block node and
// its NodeID, so the caller can pass the ID into the recursive buildBlock.
func findEntryBody(unit *tir.Unit, decl tir.Node) (tir.Node, tir.NodeID, error) {
	for _, fd := range unit.FunctionDeclarations() {
		if fd.FunctionID != decl.Function {
			continue
		}
		block, ok := unit.Node(fd.Node)
		if !ok {
			return tir.Node{}, 0, fmt.Errorf("entry function body not found in unit: FunctionDecl %d has invalid body node %d", fd.FunctionID, fd.Node)
		}
		if block.Kind != tir.Block {
			return tir.Node{}, 0, fmt.Errorf("entry function body is a %s, want a Block", block.Kind)
		}
		return block, fd.Node, nil
	}
	return tir.Node{}, 0, fmt.Errorf("entry function body declaration not found in unit: no FunctionDecl for FunctionID %d", decl.Function)
}

// validateEmptyBody accepts only a block with no statements, or the single
// synthesized ImplicitReturn that a void entry's empty body ends with. Any
// other statement content is rejected, not best-effort lowered.
func validateEmptyBody(unit *tir.Unit, block tir.Node) error {
	if len(block.Children) == 0 {
		return nil
	}
	if len(block.Children) == 1 {
		if child, ok := unit.Node(block.Children[0]); ok && child.Kind == tir.ImplicitReturn {
			return nil
		}
	}
	if child, ok := unit.Node(block.Children[0]); ok {
		return fmt.Errorf("entry function body is not empty: unsupported statement %s found; this backend only emits an empty-bodied void entry", child.Kind)
	}
	return fmt.Errorf("entry function body is not empty: %d statement(s) found; this backend only emits an empty-bodied void entry", len(block.Children))
}

// buildBlock validates one block under the entry body's recursive grammar and
// builds its C statement sequence. A block is zero or more `<cType> <width>
// pebble_local_<id>` declarations (one per Initialize, in declaration order),
// zero or more `pebble_local_<id> = <built value>;` reassignments (one per
// Store, targeting a local already in scope), and zero or more `while (...)
// { <loop body> }` loop statements (one per While, built by buildWhile — a
// loop is only ever a leading statement here, never the block's tail),
// followed by a tail that is either the single `return <expression>;` or a
// two-armed if/else built by buildIf; each if arm is itself a block under the
// same grammar, so buildBlock recurses into both arms. width is the entry's
// resolved integer width (types.I32 or types.I64), threaded through to every
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
func buildBlock(unit *tir.Unit, snapshot *types.Snapshot, blockID tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, depth int, width types.BuiltinKind) (string, error) {
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
			// A while loop is a leading statement in the block grammar, never
			// the tail: it runs its body (which may itself declare locals and
			// reassign enclosing ones) as many times as its condition holds,
			// then control falls through to the statements after it. The loop
			// body is its own scope (buildWhile clones, exactly as buildIf's
			// arms do), so nothing the loop declares leaks into this block's
			// scope map.
			whileText, err := buildWhile(unit, snapshot, statement, scope, depth, width)
			if err != nil {
				return "", err
			}
			statements = append(statements, whileText)
			continue
		}
		text, err := buildLeadingStatement(unit, snapshot, block.Children[i], scope, indent, "entry function body block", width)
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
		if len(last.Children) != 1 {
			return "", fmt.Errorf("entry function body return statement has %d argument(s), want exactly one integer expression", len(last.Children))
		}
		returnExpr, err := buildExpr(unit, snapshot, last.Children[0], scope, width)
		if err != nil {
			return "", err
		}
		statements = append(statements, indent+"return "+returnExpr+";")
	case tir.If:
		ifText, err := buildIf(unit, snapshot, last, scope, depth, width)
		if err != nil {
			return "", err
		}
		statements = append(statements, ifText)
	default:
		return "", fmt.Errorf("entry function body block statement is a %s, want a Return of an integer expression or a two-armed if/else", last.Kind)
	}
	return strings.Join(statements, "\n"), nil
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
func buildIf(unit *tir.Unit, snapshot *types.Snapshot, ifNode tir.Node, locals map[symbol.SymbolID]types.BuiltinKind, depth int, width types.BuiltinKind) (string, error) {
	if !ifNode.HasElse {
		return "", fmt.Errorf("entry function body ends with an if without an else; this backend only supports the two-armed if/else whose arms each end in one return, found an if with no else")
	}
	if len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body ends with an if with %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildBlock(unit, snapshot, ifNode.Children[1], locals, depth+1, width)
	if err != nil {
		return "", err
	}
	elseText, err := buildBlock(unit, snapshot, ifNode.Children[2], locals, depth+1, width)
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
func buildWhile(unit *tir.Unit, snapshot *types.Snapshot, whileNode tir.Node, locals map[symbol.SymbolID]types.BuiltinKind, depth int, width types.BuiltinKind) (string, error) {
	if len(whileNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop has %d child(ren), want exactly 2 (the condition, then the loop body)", len(whileNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, whileNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	bodyText, err := buildLoopBody(unit, snapshot, whileNode.Children[1], locals, depth+1, width)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	return fmt.Sprintf("%swhile (%s) {\n%s\n%s}", indent, condition, bodyText, indent), nil
}

// buildLoopBody validates and builds the C statement sequence for a while
// loop's body: a Block whose children are local declarations (Initialize),
// reassignments (Store), conditional if statements (a tir.If built by
// buildLoopIf — the else is optional in a loop body), nested while loops (a
// tir.While built by buildWhile), and break/continue statements (a tir.Break /
// tir.Continue built by buildLoopJump), built one level deeper than the
// enclosing block. A loop body has no required tail — it just runs statements
// and does not need to end in a return or if — so buildBlock is deliberately
// not reused here; the grammar is genuinely different. The body is its own
// scope: locals are cloned from the enclosing set (the same cloneLocals
// discipline buildIf's arms use) before any declaration is added, so a local
// declared inside the loop is invisible outside it and re-initializes on every
// C iteration, which is the correct C block-scope behavior for a `while cond {
// let x i32 = ...; }` shape. A nested while's body and each loop-body if arm
// are their own scopes in turn (buildWhile and buildLoopIf both recurse into
// buildLoopBody, which clones per entry), so a local declared inside one of
// them is invisible to its siblings and to anything outside it. Any other
// statement kind (a Return, a Print, anything else) is a clean rejection
// naming what was found. An empty loop body (zero children) is legal — `while
// cond {}` is a real, if useless, program — and emits no statements at all.
func buildLoopBody(unit *tir.Unit, snapshot *types.Snapshot, bodyID tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, depth int, width types.BuiltinKind) (string, error) {
	body, ok := unit.Node(bodyID)
	if !ok {
		return "", fmt.Errorf("entry function body block while loop body references invalid node %d", bodyID)
	}
	if body.Kind != tir.Block {
		return "", fmt.Errorf("entry function body block while loop body is a %s, want a Block", body.Kind)
	}
	if len(body.Children) == 0 {
		return "", nil
	}
	scope := cloneLocals(locals)
	indent := strings.Repeat("    ", depth+1)
	var statements []string
	for _, childID := range body.Children {
		statement, ok := unit.Node(childID)
		if !ok {
			return "", fmt.Errorf("entry function body block while loop body references invalid statement node %d", childID)
		}
		var text string
		var err error
		switch statement.Kind {
		case tir.While:
			// A nested while inside a loop body reuses buildWhile unchanged: it
			// already recurses into buildLoopBody for its own body, so nested
			// loops compose without any change to buildWhile itself.
			text, err = buildWhile(unit, snapshot, statement, scope, depth, width)
		case tir.If:
			// A conditional statement inside a loop body is built by buildLoopIf:
			// its arms are themselves loop bodies (no required tail, optional
			// else), genuinely different from the tail-requiring buildIf. Because
			// buildLoopIf recurses into buildLoopBody for each arm, a break or
			// continue inside an arm is handled by this same switch, unchanged.
			text, err = buildLoopIf(unit, snapshot, statement, scope, depth, width)
		case tir.Break:
			text, err = buildLoopJump(statement, "break", indent, "entry function body block while loop body")
		case tir.Continue:
			text, err = buildLoopJump(statement, "continue", indent, "entry function body block while loop body")
		default:
			text, err = buildLeadingStatement(unit, snapshot, childID, scope, indent, "entry function body block while loop body", width)
		}
		if err != nil {
			return "", err
		}
		statements = append(statements, text)
	}
	return strings.Join(statements, "\n"), nil
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
func buildLoopIf(unit *tir.Unit, snapshot *types.Snapshot, ifNode tir.Node, locals map[symbol.SymbolID]types.BuiltinKind, depth int, width types.BuiltinKind) (string, error) {
	if ifNode.HasElse && len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body block while loop body if has an else arm but %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	if !ifNode.HasElse && len(ifNode.Children) != 2 {
		return "", fmt.Errorf("entry function body block while loop body if has no else arm but %d child(ren), want exactly 2 (condition, then-arm)", len(ifNode.Children))
	}
	condition, err := buildCondition(unit, snapshot, ifNode.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	thenText, err := buildLoopBody(unit, snapshot, ifNode.Children[1], locals, depth+1, width)
	if err != nil {
		return "", err
	}
	indent := strings.Repeat("    ", depth+1)
	if !ifNode.HasElse {
		return fmt.Sprintf("%sif (%s) {\n%s\n%s}", indent, condition, thenText, indent), nil
	}
	elseText, err := buildLoopBody(unit, snapshot, ifNode.Children[2], locals, depth+1, width)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("%sif (%s) {\n%s\n%s} else {\n%s\n%s}", indent, condition, thenText, indent, elseText, indent), nil
}

// buildLoopJump validates and builds the C text for one break/continue
// statement in a loop body. A tir.Break or tir.Continue is a leaf node (no
// children, confirmed against real fixtures) whose Target names the region of
// the loop the jump leaves, and whose DeferChain would carry the DeferRegister
// nodes this backend would have to expand before the jump if the loop body had
// any `defer` statements. This backend does not lower defer at all yet, so a
// non-empty DeferChain is a shape it cannot correctly emit — it is rejected
// cleanly, naming the chain length, never silently dropped. (The checker
// accepts `defer` inside a loop body today, so real source does produce
// non-empty chains on a jump that crosses a deferred region.) The emitted C is
// exactly `break;` / `continue;` at the current indent: the language has no
// labeled break/continue, so a jump's Target always names the nearest enclosing
// loop and plain C break/continue — which already target the nearest enclosing
// loop by C's own scoping rules — is a direct, correct translation. No runtime
// helper is involved, and Target's value never needs to be consulted or
// compared; it is confirmed (against a nested-loop fixture) to name the loop
// that actually contains the jump, and the checker (C0611) already guarantees
// that loop is an enclosing one.
func buildLoopJump(statement tir.Node, keyword string, indent, context string) (string, error) {
	if len(statement.DeferChain) != 0 {
		return "", fmt.Errorf("%s %s statement carries %d deferred statement(s) in its DeferChain, which this backend does not support (defer is not lowered yet)", context, keyword, len(statement.DeferChain))
	}
	return fmt.Sprintf("%s%s;", indent, keyword), nil
}

// buildLeadingStatement validates and builds one leading statement in the
// block grammar shared by buildBlock and buildLoopBody: an Initialize (a local
// declaration) or a Store (a reassignment of a local already in scope).
// context names the enclosing construct in error messages; indent is the
// statement's C indentation. scope is the set of in-scope locals, each mapped
// to the resolved builtin type it was declared with (the entry's integer width
// or bool): an Initialize adds its symbol to it once validated, a Store reads
// it. width is
// the entry's resolved integer width; an integer local's C type name
// follows it (int32_t for an i32 entry, int64_t for an i64 entry), and a
// local whose value carries the other width is rejected by buildExpr, so an
// i32 local inside an i64 entry (or vice versa) is a clean width-mismatch
// error, not an attempted coercion. A local whose value carries the bool
// builtin is a bool local, declared as C `bool` and built by buildBoolExpr;
// its scope entry records types.Bool so a later reference or reassignment is
// emitted and validated against the same type. The
// caller is responsible for having already cloned scope if the statements must
// not leak into a sibling or enclosing scope (buildBlock and buildLoopBody both
// do). Any other statement kind is a clean rejection naming what was found.
func buildLeadingStatement(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, scope map[symbol.SymbolID]types.BuiltinKind, indent, context string, width types.BuiltinKind) (string, error) {
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
		kind, ok := resolvedBuiltin(snapshot, initValue.Type)
		if !ok {
			return "", fmt.Errorf("%s local declaration declares a local of type %s, want %s or bool", context, describeType(snapshot, initValue.Type), wantName(width))
		}
		switch kind {
		case width:
			// An integer local: emitted at the entry's width, exactly as
			// before (buildExpr re-checks every node in the initializer is
			// that width). The scope entry records the width so a later
			// reference or reassignment is validated and emitted as an
			// integer.
			initExpr, err := buildExpr(unit, snapshot, statement.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			scope[statement.Symbol] = width
			return fmt.Sprintf("%s%s pebble_local_%d = %s;", indent, cType(width), statement.Symbol, initExpr), nil
		case types.Bool:
			// A bool local: emitted as a C bool. The bool value grammar is
			// genuinely different from the integer one (no checked
			// arithmetic), so it is built by buildBoolExpr, not buildExpr.
			initExpr, err := buildBoolExpr(unit, snapshot, statement.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			scope[statement.Symbol] = types.Bool
			// Like integer locals (see the width case), a bool local is
			// emitted as a plain (non-const) bool: the Initialize node does
			// not carry let-vs-var, and the checker guarantees any Store
			// this backend sees targets a writable `var`, so const would
			// only be defense-in-depth at the cost of tracking which locals
			// are ever reassigned.
			return fmt.Sprintf("%sbool pebble_local_%d = %s;", indent, statement.Symbol, initExpr), nil
		default:
			return "", fmt.Errorf("%s local declaration declares a local of type %s, want %s or bool", context, describeType(snapshot, initValue.Type), wantName(width))
		}
	case tir.Store:
		// A Store reassigns a local declared earlier in this block or an
		// enclosing one; it does not declare a new symbol, so it never
		// touches scope. The checker refuses to emit a Store targeting a
		// `let` (C0606: the assignment place is not writable), so any
		// Store this backend sees, from real source, necessarily targets
		// a `var`.
		if len(statement.Children) != 2 {
			return "", fmt.Errorf("%s reassignment has %d child(ren), want exactly two: the place being reassigned and the new value", context, len(statement.Children))
		}
		place, ok := unit.Node(statement.Children[0])
		if !ok {
			return "", fmt.Errorf("%s reassignment references invalid place node %d", context, statement.Children[0])
		}
		if place.Kind != tir.StoragePlace {
			return "", fmt.Errorf("%s reassignment targets a %s, want a plain StoragePlace naming a local in scope", context, place.Kind)
		}
		targetKind, declared := scope[place.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reassigns symbol %d, which is not a local in scope", context, place.Symbol)
		}
		// The new value is validated and emitted against the local's own
		// declared type: the entry's width for an integer local (buildExpr),
		// the bool grammar for a bool local (buildBoolExpr). A value of the
		// wrong type — a bool assigned to an integer local, or an integer
		// assigned to a bool local — is rejected by the appropriate builder.
		switch targetKind {
		case width:
			storeValue, err := buildExpr(unit, snapshot, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%spebble_local_%d = %s;", indent, place.Symbol, storeValue), nil
		case types.Bool:
			storeValue, err := buildBoolExpr(unit, snapshot, statement.Children[1], scope, width)
			if err != nil {
				return "", err
			}
			return fmt.Sprintf("%spebble_local_%d = %s;", indent, place.Symbol, storeValue), nil
		default:
			return "", fmt.Errorf("%s reassigns symbol %d, which is a local of type %s, want %s or bool", context, place.Symbol, describeType(snapshot, place.Type), wantName(width))
		}
	default:
		return "", fmt.Errorf("%s statement is a %s, want a local declaration (Initialize) or a reassignment (Store)", context, statement.Kind)
	}
}

// cloneLocals returns a fresh copy of the given set of in-scope locals. Every
// recursive scope entry in buildBlock copies before extending, so a block's
// own declarations never leak into the map the caller or a sibling scope
// sees — a local declared inside one if arm is invisible to the sibling arm
// and to anything outside the arm.
func cloneLocals(locals map[symbol.SymbolID]types.BuiltinKind) map[symbol.SymbolID]types.BuiltinKind {
	cloned := make(map[symbol.SymbolID]types.BuiltinKind, len(locals))
	for id, kind := range locals {
		cloned[id] = kind
	}
	return cloned
}

// buildCondition builds the C text for one if/while condition. It dispatches
// on the condition node's shape: a direct integer comparison (tir.BinaryValue)
// keeps the existing buildComparison path unchanged, while a bare bool value —
// a bool literal, a reference to an in-scope bool local, a unary ! negation of
// one of those (tir.PrefixValue with the Bang operator), a comparison used as
// a bool operand, or a && / || combination of any of these (a
// tir.ShortCircuitValue) — is routed through buildBoolExpr. Anything else is
// rejected by whichever builder it reaches.
func buildCondition(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body condition references invalid node %d", id)
	}
	if node.Kind == tir.BinaryValue {
		return buildComparison(unit, snapshot, id, locals, width)
	}
	return buildBoolExpr(unit, snapshot, id, locals, width)
}

// buildComparison builds the C text for an if condition. It accepts exactly a
// tir.BinaryValue with two operands and one of the six comparison operators
// (<, <=, >, >=, ==, !=), and emits the plain C operator directly — comparing
// two integers, or two bools with ==/!=, cannot overflow, so no runtime helper
// is needed. The operand grammar is decided from the operands' own resolved
// types, not assumed to be integers: when both operands carry the snapshot's
// bool builtin, they are built by buildBoolExpr (a bool comparison result, a
// bool local, a bool literal, a ! negation, or a && / || combination — the
// wrapped-comparison shape (1 < 2) == (3 < 4) is exactly this, its two
// SourceAlias-wrapped comparison operands being bool values), and only the
// ==/!= operators are legal for bool operands — the checker itself rejects an
// ordering comparison between bools (C0603, confirmed against a real fixture),
// so that ordering guard is defense for hand-built IR, not a reachable source
// shape. Both bool operands are parenthesized in the emitted C so a bool
// operand that is itself a comparison cannot chain associatively with the
// outer operator (e.g. (a == b) == (c == d) must not collapse to a left-to-
// right a == b == c == d). Otherwise each operand is built by
// buildComparisonOperand (an int-typed integer literal, or any i32 expression
// buildExpr accepts). Any other node kind, or any other operator on a
// BinaryValue (bitwise), is a clean rejection. The && / || that lower to
// ShortCircuitValue nodes are not this function's concern — buildCondition
// routes them to buildBoolExpr.
func buildComparison(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid node %d", id)
	}
	if node.Kind != tir.BinaryValue {
		return "", fmt.Errorf("entry function body if condition is a %s, want a direct integer comparison or a ==/!= between two bool values (<, <=, >, >=, ==, or !=)", node.Kind)
	}
	if len(node.Children) != 2 {
		return "", fmt.Errorf("entry function body if condition has %d operand(s), want exactly two operands", len(node.Children))
	}
	op, ok := comparisonOperator(node.Operator)
	if !ok {
		return "", fmt.Errorf("entry function body if condition uses operator %s, want one of <, <=, >, >=, ==, or !=", node.Operator)
	}
	leftOperand, ok := unit.Node(node.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[0])
	}
	rightOperand, ok := unit.Node(node.Children[1])
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", node.Children[1])
	}
	if isBool(snapshot, leftOperand.Type) && isBool(snapshot, rightOperand.Type) {
		// Both operands are bool values, so this is an equality between bools
		// — (1 < 2) == (3 < 4), a == b, true == a, and so on. Only ==/!= make
		// sense for bool operands; an ordering comparison here is impossible
		// from real source (the checker rejects it as C0603 before typed IR
		// exists), but is rejected cleanly rather than guessed for hand-built
		// IR. The operands are built under the bool grammar by buildBoolExpr,
		// each parenthesized so a comparison operand cannot chain associatively
		// with the outer operator in the emitted C.
		if node.Operator != syntax.Equal && node.Operator != syntax.NotEqual {
			return "", fmt.Errorf("entry function body if condition compares two bool operands with operator %s, want == or !=", node.Operator)
		}
		left, err := buildBoolExpr(unit, snapshot, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(unit, snapshot, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + ") " + op + " (" + right + ")", nil
	}
	left, err := buildComparisonOperand(unit, snapshot, node.Children[0], locals, width)
	if err != nil {
		return "", err
	}
	right, err := buildComparisonOperand(unit, snapshot, node.Children[1], locals, width)
	if err != nil {
		return "", err
	}
	return left + " " + op + " " + right, nil
}

// buildComparisonOperand builds one comparison operand. A bare comparison
// between two untyped integer literals defaults both operands to the
// snapshot's int builtin (confirmed against a real fixture — the same for an
// i64 entry as for an i32 one, since a bare comparison has no anchor), so an
// IntegerLiteral of type int is lowered directly as its decimal text. Every
// other operand must be an expression of the entry's width that buildExpr
// accepts — a literal, a
// reference to a local declared earlier in the entry body, or checked negation
// and checked +, -, *, /, % arithmetic — and is delegated to buildExpr, whose
// own width gate and kind switch do the rejecting.
func buildComparisonOperand(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid operand node %d", id)
	}
	if node.Kind == tir.IntegerLiteral && node.Type == snapshot.Builtins().Int {
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body if condition contains an integer literal with malformed text %q", text)
		}
		return text, nil
	}
	return buildExpr(unit, snapshot, id, locals, width)
}

// comparisonOperator maps the six comparison token kinds this backend lowers
// to their plain C spellings. These map 1:1 to C syntax — no runtime helper is
// involved, since comparing two integer values (the entry's width, or the
// int-typed literal case) cannot overflow. Any other
// operator is deliberately not mapped and rejected by the caller.
func comparisonOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Less:
		return "<", true
	case syntax.LessEqual:
		return "<=", true
	case syntax.Greater:
		return ">", true
	case syntax.GreaterEqual:
		return ">=", true
	case syntax.Equal:
		return "==", true
	case syntax.NotEqual:
		return "!=", true
	default:
		return "", false
	}
}

// buildExpr builds the C expression text for an integer value node of the
// entry's resolved width, recursing into its operands. width (types.I32 or
// types.I64) is the width resolved once in Emit; every node in an accepted
// tree must carry exactly that width's builtin — a node carrying the other
// width (an i32 local referenced inside an i64 entry, or vice versa) is a
// clean width-mismatch rejection, never a coercion. locals is the set of
// symbols in scope at this point in the
// entry body (a map is deliberately used, not a slice, so membership is a
// constant-time check); it is read-only for a SymbolValue reference and is
// otherwise threaded through unchanged. It accepts exactly four node kinds:
//
//   - IntegerLiteral — its decimal text (defensively validated, exactly as
//     10.3 validated a bare literal return).
//   - CheckedNegate with exactly one operand of the entry's width —
//     pebble_rt_checked_neg_<suffix>.
//   - CheckedArithmetic with exactly two operands of the entry's width and
//     operator +, -, *, /, or % — pebble_rt_checked_add_<suffix> /
//     pebble_rt_checked_sub_<suffix> / pebble_rt_checked_mul_<suffix> /
//     pebble_rt_checked_div_<suffix> / pebble_rt_checked_mod_<suffix>.
//   - SymbolValue whose Symbol is in locals — pebble_local_<symbol ID>, the C
//     name buildBlock gave that local's declaration.
//
// CheckedArithmetic with any other operator (the integral operators that build
// this node but are not yet lowered) is rejected, not guessed. A SymbolValue
// referencing anything not in locals (a global, a parameter, a symbol from an
// outer/different scope — none of which are reachable from this narrow body
// shape, but checked defensively rather than assumed) is a clean rejection.
// Any other node kind at any position — a function call, a non-integer
// operand, CheckedShift, and so on — is a clean rejection naming what was
// found.
// Emitting the checked runtime helpers (rather than raw C operators) is what
// keeps the IR nodes' real overflow and divide-by-zero semantics from silently
// disappearing in the emitted program.
func buildExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isWidth(snapshot, width, node.Type) {
		wantName, _ := builtinName(width)
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want %s", node.Kind, describeType(snapshot, node.Type), wantName)
	}
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function body expression contains an integer literal with malformed text %q", text)
		}
		return text, nil
	case tir.CheckedNegate:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function body expression contains a CheckedNegate with operator %s, want -", node.Operator)
		}
		child, err := buildExpr(unit, snapshot, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "pebble_rt_checked_neg_" + checkedSuffix(width) + "(" + child + ")", nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedArithmeticHelper(node.Operator, width)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with operator %s, want +, -, *, /, or %%", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(unit, snapshot, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return helper + "(" + left + ", " + right + ")", nil
	case tir.SymbolValue:
		if _, declared := locals[node.Symbol]; !declared {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an integer literal, a reference to a local declared earlier in the body, or checked +, -, *, /, %% arithmetic", node.Kind)
	}
}

// buildBoolExpr builds the C text for a bool value node, used both for a bool
// local's initializer/reassignment value and for a bare bool if/while
// condition (via buildCondition). The bool grammar is genuinely different from
// the integer one buildExpr handles: there is no checked arithmetic — bools
// are combined, compared, and negated with plain C, which cannot fault — so it
// is a separate builder rather than a mode on buildExpr. width is the entry's
// resolved integer width, threaded through to the comparison path so a
// comparison used as a bool value's operand builds its own integer operands at
// the entry's width. It accepts exactly seven node kinds, each carrying the
// snapshot's bool builtin:
//
//   - BoolLiteral — the C literal true/false (requires #include <stdbool.h>).
//   - SymbolValue whose Symbol is a bool local in scope (the locals map
//     records types.Bool for it) — pebble_local_<symbol ID>, the same C name
//     buildLeadingStatement gave that local's declaration.
//   - PrefixValue with operator ! (syntax.Bang, confirmed against a real
//     fixture — a bool `!` is a PrefixValue, not the CheckedNegate integer
//     negation uses) and exactly one operand that is itself a bool value in
//     this grammar — !(<operand>), plain C negation. The operand is built by
//     recursing into this same builder, so a negated comparison (e.g.
//     !(i < 5)) is now accepted: its operand is a SourceAlias wrapping a
//     BinaryValue, both handled below.
//   - BinaryValue with one of the six comparison operators — delegated to
//     buildComparison, the same path a top-level if/while condition uses, so
//     a comparison can serve as an operand of && / || as well as stand alone.
//     buildComparison decides the operand grammar from the operands' resolved
//     types: integer operands take the integer comparison path, and two bool
//     operands — e.g. (1 < 2) == (3 < 4), whose SourceAlias-wrapped
//     comparison operands are bool values — take the bool-equality path.
//     (A BinaryValue with any other operator is rejected by buildComparison's
//     operator check.)
//   - ShortCircuitValue with operator && (syntax.LogicalAnd) or ||
//     (syntax.LogicalOr) — <(left) && (right)> / <(left) || (right)>,
//     parenthesized so nested combinations produce unambiguous C regardless of
//     depth. Both operands are built by recursing into this same builder, so
//     && and || combine literals, bool locals, ! negations, comparisons, and
//     nested && / || freely. Plain C && and || are the correct lowering: both
//     languages short-circuit, and every operand this builder produces is
//     side-effect-free (no calls, no mutation inside an expression), so
//     nothing observable changes whether or not the right operand is
//     evaluated. The operand tree already encodes the language's &&-vs-||
//     precedence (confirmed: Pebble's grammar gives || precedence 1 and &&
//     precedence 2), so this builder never re-derives precedence.
//   - SourceAlias — a transparent wrapper (the grouped-expression parens), so
//     it is unwrapped and its single child built by recursing into this same
//     builder. A parenthesized comparison operand of && / || is exactly this
//     shape (confirmed against a real fixture: flag && (1 < 2) has the
//     comparison wrapped in a SourceAlias, while the unparenthesized
//     1 < 2 && 3 < 4 wraps nothing).
//
// A SymbolValue referencing anything else — an integer local, a global, a
// parameter — and any other node kind at any position is a clean rejection
// naming what was found.
func buildBoolExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]types.BuiltinKind, width types.BuiltinKind) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isBool(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want bool", node.Kind, describeType(snapshot, node.Type))
	}
	switch node.Kind {
	case tir.BoolLiteral:
		if node.Literal.Bool {
			return "true", nil
		}
		return "false", nil
	case tir.SymbolValue:
		if locals[node.Symbol] != types.Bool {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a bool local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.PrefixValue:
		if node.Operator != syntax.Bang {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with operator %s, want !", node.Operator)
		}
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a PrefixValue with %d operand(s), want exactly one", len(node.Children))
		}
		child, err := buildBoolExpr(unit, snapshot, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		return "!(" + child + ")", nil
	case tir.BinaryValue:
		// A comparison used as a bool value (an operand of && / ||, or the
		// condition routed here by buildCondition) is the same BinaryValue
		// shape buildComparison already lowers for a top-level condition, so it
		// is delegated unchanged. Non-comparison operators and non-integer
		// operands are rejected by buildComparison itself.
		return buildComparison(unit, snapshot, id, locals, width)
	case tir.ShortCircuitValue:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with %d operand(s), want exactly two", len(node.Children))
		}
		op, ok := shortCircuitOperator(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a ShortCircuitValue with operator %s, want && or ||", node.Operator)
		}
		left, err := buildBoolExpr(unit, snapshot, node.Children[0], locals, width)
		if err != nil {
			return "", err
		}
		right, err := buildBoolExpr(unit, snapshot, node.Children[1], locals, width)
		if err != nil {
			return "", err
		}
		return "(" + left + " " + op + " " + right + ")", nil
	case tir.SourceAlias:
		// A SourceAlias is transparent — it records grouped-expression parens
		// and nothing else — so it is unwrapped and its single child built.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		return buildBoolExpr(unit, snapshot, node.Children[0], locals, width)
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a bool literal, a reference to a bool local declared earlier in the body, a comparison, a && / || combination, or a ! negation", node.Kind)
	}
}

// shortCircuitOperator maps the two logical-combination token kinds a
// ShortCircuitValue may carry to their plain C spellings. Both C and Pebble
// && and || short-circuit their right operand, and both sides of the operator
// are side-effect-free in this backend's grammar, so the plain C operator is a
// direct, correct lowering. Any other operator is deliberately not mapped and
// rejected by the caller.
func shortCircuitOperator(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.LogicalAnd:
		return "&&", true
	case syntax.LogicalOr:
		return "||", true
	default:
		return "", false
	}
}

// checkedArithmeticHelper maps the +, -, *, /, % operators a CheckedArithmetic
// node may carry to the runtime helper that implements their checked semantics,
// at the entry's resolved width (width's checkedSuffix picks the _i32 or _i64
// function-name suffix).
// Division and modulo map to pebble_rt_checked_div_i32 / pebble_rt_checked_mod_i32
// (or their _i64 twins), which handle both the divide-by-zero fault (in every
// mode) and the one
// division overflow input, INT32_MIN / -1 (INT64_MIN / -1 at the wider
// width). Any other operator (bitwise, etc.)
// is deliberately not mapped and rejected by the caller.
func checkedArithmeticHelper(op syntax.TokenKind, width types.BuiltinKind) (string, bool) {
	var base string
	switch op {
	case syntax.Plus:
		base = "pebble_rt_checked_add"
	case syntax.Minus:
		base = "pebble_rt_checked_sub"
	case syntax.Star:
		base = "pebble_rt_checked_mul"
	case syntax.Slash:
		base = "pebble_rt_checked_div"
	case syntax.Percent:
		base = "pebble_rt_checked_mod"
	default:
		return "", false
	}
	return base + "_" + checkedSuffix(width), true
}

// isWidth reports whether id is the snapshot's builtin for the entry's
// resolved integer width (types.I32 or types.I64). The checked helpers this
// backend emits operate on exactly one width per entry, so every node in an
// accepted expression tree must carry exactly this type — a node carrying the
// other width is a clean rejection, never a coercion, since there is no
// cast/coercion lowering yet.
func isWidth(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	var want types.TypeID
	switch width {
	case types.I32:
		want = snapshot.Builtins().I32
	case types.I64:
		want = snapshot.Builtins().I64
	default:
		return false
	}
	return id == want
}

// isBool reports whether id is the snapshot's bool builtin. It is the bool
// twin of isWidth: every node in an accepted bool expression tree must carry
// exactly the bool builtin, since this backend has no cast/coercion lowering
// between bool and anything else.
func isBool(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Bool
}

// resolvedBuiltin resolves a TypeID to the builtin kind it names, if it names
// one. It is how the emitter decides what a value node's type means — the
// entry's integer width for an integer local's initializer, or bool for a bool
// local's — without re-deriving anything.
func resolvedBuiltin(snapshot *types.Snapshot, id types.TypeID) (types.BuiltinKind, bool) {
	if snapshot == nil {
		return 0, false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return 0, false
	}
	return key.Builtin()
}

// wantName returns the human-readable name of the entry's resolved integer
// width ("i32" or "i64") for error messages that name the wanted type.
func wantName(width types.BuiltinKind) string {
	name, _ := builtinName(width)
	return name
}

// cType returns the C type name an integer local of the given width is
// declared with: int32_t for an i32 entry, int64_t for an i64 entry. Only the
// two widths this backend emits are mapped; anything else returns "" and the
// caller's own width validation has already ruled it out.
func cType(width types.BuiltinKind) string {
	switch width {
	case types.I32:
		return "int32_t"
	case types.I64:
		return "int64_t"
	}
	return ""
}

// checkedSuffix returns the pebble_rt_checked_* function-name suffix for the
// given width: "i32" for an i32 entry, "i64" for an i64 entry. It is exactly
// the type's name, but named for what it selects — the width-specific runtime
// helper family — so the two lookups stay distinct.
func checkedSuffix(width types.BuiltinKind) string {
	switch width {
	case types.I32:
		return "i32"
	case types.I64:
		return "i64"
	}
	return ""
}

// isNonNegativeDecimal reports whether s is a non-empty run of ASCII decimal
// digits with no sign prefix. IntegerNum is expected to be exactly this by
// construction; the check is defensive against malformed payloads.
func isNonNegativeDecimal(s string) bool {
	if s == "" {
		return false
	}
	for i := 0; i < len(s); i++ {
		if s[i] < '0' || s[i] > '9' {
			return false
		}
	}
	return true
}

// The supported entry shapes share one adapter skeleton: the pebble_rt.h
// include, the Pebble-convention pebble_user_main taking the context, and a
// hosted C main that builds a default context and drives it. Only the
// pebble_user_main definition and the hosted main's final call differ between
// the void and i32 shapes, so the skeleton is written once and the two
// fragments below are the only shape-specific text.
const voidEntryUserMain = `static void pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
}`

const voidEntryMainBody = `pebble_user_main(&ctx);
    return 0;`

// integerEntryUserMain is a format string; the first %s is the pebble_user_main
// return type for the entry's resolved width (entryReturnType) and the second
// %s is the statement sequence for
// pebble_user_main's body — the top-level block built by buildBlock: zero or
// more `<width> pebble_local_<id> = <built init expression>;` declarations and
// zero or more `pebble_local_<id> = <built value>;` reassignments, in
// declaration order, then the block's tail, which is either a
// `return <built return expression>;` or a two-armed if/else (whose arms may
// nest further blocks). The tail's value becomes pebble_user_main's return
// value and, through the hosted main's own return, the process exit code. With
// no locals the sequence is exactly the single return statement, so the
// zero-locals shape emits byte-identically to before for an i32 entry (whose
// return type is the legacy "int").
const integerEntryUserMain = `static %s pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
%s
}`

const integerEntryMainBody = `return pebble_user_main(&ctx);`

// entryReturnType is the C return type pebble_user_main is declared with for
// a supported integer width. An i32 entry keeps the legacy "int" spelling —
// byte-identical to the pre-i64 shape, and C int is the 32-bit type that entry
// already relied on. An i64 entry must be the exact-width int64_t, not int, so
// a 64-bit return value is not truncated to 32 bits before the hosted main
// narrows it to the process exit code. (The hosted int main's own
// return pebble_user_main(&ctx); then narrows int64_t to int — the POSIX exit
// code is only the low byte of what main returns — which a -Wall -Wextra
// -Werror build without -Wconversion does not warn about; verified by building
// an i64-entry program.)
func entryReturnType(width types.BuiltinKind) string {
	if width == types.I64 {
		return "int64_t"
	}
	return "int"
}

// emitEntryC writes the shared adapter skeleton once the typed IR has been
// confirmed to describe one of the two supported program shapes. <stdbool.h>
// is included unconditionally: it provides the C bool keyword and the true /
// false literals the moment any bool local or literal is emitted, and adding
// it for programs with no bool at all is harmless.
func emitEntryC(w io.Writer, userMain, mainBody string) error {
	_, err := fmt.Fprintf(w, `#include "pebble_rt.h"
#include <stdbool.h>

%s

int main(int argc, const char **argv) {
    (void)argc;
    (void)argv;
    PebbleContext ctx = pebble_rt_default_context();
    %s
}
`, userMain, mainBody)
	return err
}

func callingConventionName(c types.CallingConvention) string {
	switch c {
	case types.Pebble:
		return "Pebble"
	case types.C:
		return "C"
	default:
		return fmt.Sprintf("calling-convention(%d)", uint8(c))
	}
}

// describeType renders a TypeID into a short human-readable spelling for
// error messages. It only needs to be good enough to name what was found.
func describeType(snapshot *types.Snapshot, id types.TypeID) string {
	if snapshot == nil {
		return fmt.Sprintf("type %d", id)
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return fmt.Sprintf("type %d", id)
	}
	switch key.Kind() {
	case types.Builtin:
		if builtin, ok := key.Builtin(); ok {
			if name, ok := builtinName(builtin); ok {
				return name
			}
		}
	case types.Pointer:
		if child, ok := key.Child(); ok {
			return "*" + describeType(snapshot, child)
		}
	case types.Array:
		if length, child, ok := key.Array(); ok {
			return fmt.Sprintf("[%d]%s", length, describeType(snapshot, child))
		}
	case types.Slice:
		if child, ok := key.Child(); ok {
			return "[]" + describeType(snapshot, child)
		}
	case types.Tuple:
		if elements, ok := key.Elements(); ok {
			parts := make([]string, len(elements))
			for i, element := range elements {
				parts[i] = describeType(snapshot, element)
			}
			return "(" + strings.Join(parts, ", ") + ")"
		}
	case types.Optional:
		if child, ok := key.Child(); ok {
			return "?" + describeType(snapshot, child)
		}
	case types.Function:
		if _, parameters, result, _, ok := key.Function(); ok {
			parts := make([]string, len(parameters))
			for i, parameter := range parameters {
				parts[i] = describeType(snapshot, parameter)
			}
			return "fn(" + strings.Join(parts, ", ") + ") " + describeType(snapshot, result)
		}
	case types.Nominal:
		if declaration, _, ok := key.Nominal(); ok {
			return fmt.Sprintf("nominal(symbol %d)", declaration)
		}
	case types.TypeParameter:
		if declaration, ok := key.TypeParameter(); ok {
			return fmt.Sprintf("type-parameter(symbol %d)", declaration)
		}
	}
	return fmt.Sprintf("type %d", id)
}

func builtinName(builtin types.BuiltinKind) (string, bool) {
	switch builtin {
	case types.Bool:
		return "bool", true
	case types.Char:
		return "char", true
	case types.Str:
		return "str", true
	case types.Void:
		return "void", true
	case types.Int:
		return "int", true
	case types.Uint:
		return "uint", true
	case types.I8:
		return "i8", true
	case types.I16:
		return "i16", true
	case types.I32:
		return "i32", true
	case types.I64:
		return "i64", true
	case types.U8:
		return "u8", true
	case types.U16:
		return "u16", true
	case types.U32:
		return "u32", true
	case types.U64:
		return "u64", true
	case types.F32:
		return "f32", true
	case types.F64:
		return "f64", true
	}
	return "", false
}
