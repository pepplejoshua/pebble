// Package backend lowers typed IR to C source emitted against the versioned
// runtime ABI (runtime/include/pebble_rt.h). It is deliberately narrow: the
// current slice emits exactly four program shapes — an empty-bodied Pebble-
// convention void entry function; a zero-parameter i32 entry whose body is
// exactly one `return <i32 expression>;` where the expression is a small tree
// of integer literals, checked negation, and checked +, -, *, /, %
// arithmetic; a zero-parameter i32 entry whose body is zero or more
// `let <name> i32 = <i32 expression>;` local declarations followed by exactly
// one `return <i32 expression>;`, where any expression may reference a local
// declared earlier in the same body; and the same body shapes where the final
// statement is instead a two-armed `if <comparison> { return <expr>; } else {
// return <expr>; }` whose condition is a direct i32 comparison and whose arms
// each hold exactly one return — and rejects everything else with a
// descriptive error instead of guessing.
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
// {}` produces) or i32 with a body of zero or more `let <name> i32 =
// <i32 expression>;` local declarations followed by a final statement that is
// either exactly one `return <i32 expression>;` statement or a two-armed
// `if <comparison> { return <expr>; } else { return <expr>; }` where the
// condition is a direct i32 comparison (<, <=, >, >=, ==, !=) and each arm is
// exactly one return; in every case the final expression's value is propagated
// as the process's exit code. Each expression — a local's initializer, a
// return value, or an if/else arm's return value — may be a plain non-negative
// integer literal, a tree of checked negation and checked +, -, *, /, %
// arithmetic (see buildExpr), or a reference to any local declared earlier in
// the same body, visible in the condition and both arms alike. A comparison's
// operands are additionally allowed to be int-typed integer literals (see
// buildComparisonOperand). Checked operations emit pebble_rt_checked_*_i32
// calls so the language's overflow and divide-by-zero semantics survive into
// the emitted program; comparisons emit the plain C operator, which cannot
// overflow. Any other shape returns a descriptive error and writes nothing to
// w; this package does not yet lower arbitrary expressions or statements.
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
	block, err := findEntryBody(unit, decl)
	if err != nil {
		return err
	}
	if result == types.Void {
		if err := validateEmptyBody(unit, block); err != nil {
			return err
		}
		return emitEntryC(w, voidEntryUserMain, voidEntryMainBody)
	}
	statements, err := buildIntegerEntryStatements(unit, snapshot, block)
	if err != nil {
		return err
	}
	return emitEntryC(w, fmt.Sprintf(integerEntryUserMain, statements), integerEntryMainBody)
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
// count, and result type against the two supported shapes: a void result
// (empty body) or an i32 result (single literal return). On success it returns
// the resolved result builtin (types.Void or types.I32); whether the body
// actually matches the result's shape is decided by the body-validation step
// the caller dispatches on.
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
	if !ok || (builtin != types.Void && builtin != types.I32) {
		return 0, fmt.Errorf("entry function result type is %s, want void or i32", describeType(snapshot, decl.ResultType))
	}
	return builtin, nil
}

// findEntryBody follows the entry declaration's FunctionID to its FunctionDecl
// and resolves that declaration's body node. The body node is a distinct
// Block entry in unit.Nodes(), separate from the FunctionDeclaration node
// found by findEntryDeclaration.
func findEntryBody(unit *tir.Unit, decl tir.Node) (tir.Node, error) {
	for _, fd := range unit.FunctionDeclarations() {
		if fd.FunctionID != decl.Function {
			continue
		}
		block, ok := unit.Node(fd.Node)
		if !ok {
			return tir.Node{}, fmt.Errorf("entry function body not found in unit: FunctionDecl %d has invalid body node %d", fd.FunctionID, fd.Node)
		}
		if block.Kind != tir.Block {
			return tir.Node{}, fmt.Errorf("entry function body is a %s, want a Block", block.Kind)
		}
		return block, nil
	}
	return tir.Node{}, fmt.Errorf("entry function body declaration not found in unit: no FunctionDecl for FunctionID %d", decl.Function)
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

// buildIntegerEntryStatements validates the i32 entry's supported body and
// builds the C statement sequence for it: zero or more `const int32_t
// pebble_local_<id>` declarations (one per local, in declaration order)
// followed by a final statement that is either the single `return <i32
// expression>;` or a two-armed if/else built by buildIfElseTail. The accepted
// body: a block whose children are zero or more Initialize statements followed
// by exactly one final statement. A local's initializer, the return's value,
// and each arm's return value are all built by buildExpr, so each is a plain
// non-negative integer literal, a tree of checked negation and checked +, -, *,
// /, % arithmetic, or a reference to a local declared earlier in the same body.
// Each local's symbol ID names its C declaration directly, so the emitted name
// is deterministic and derived from stable IR identity rather than an
// emission-time counter. Any other shape is rejected with a descriptive error,
// not best-effort lowered.
func buildIntegerEntryStatements(unit *tir.Unit, snapshot *types.Snapshot, block tir.Node) (string, error) {
	if len(block.Children) == 0 {
		return "", fmt.Errorf("entry function body is empty, want zero or more local declarations followed by exactly one return of an integer expression or an if/else whose arms each return one")
	}
	locals := make(map[symbol.SymbolID]bool)
	var statements []string
	for i := 0; i < len(block.Children)-1; i++ {
		init, ok := unit.Node(block.Children[i])
		if !ok {
			return "", fmt.Errorf("entry function body references invalid statement node %d", block.Children[i])
		}
		if init.Kind != tir.Initialize {
			return "", fmt.Errorf("entry function body statement is a %s, want a local declaration (Initialize) before the final return or if/else", init.Kind)
		}
		if len(init.Children) != 1 {
			return "", fmt.Errorf("entry function body local declaration initializes %d value(s), want exactly one i32 expression", len(init.Children))
		}
		if locals[init.Symbol] {
			return "", fmt.Errorf("entry function body declares local %d more than once", init.Symbol)
		}
		initExpr, err := buildExpr(unit, snapshot, init.Children[0], locals)
		if err != nil {
			return "", err
		}
		locals[init.Symbol] = true
		statements = append(statements, fmt.Sprintf("    const int32_t pebble_local_%d = %s;", init.Symbol, initExpr))
	}
	last, ok := unit.Node(block.Children[len(block.Children)-1])
	if !ok {
		return "", fmt.Errorf("entry function body references invalid statement node %d", block.Children[len(block.Children)-1])
	}
	switch last.Kind {
	case tir.Return:
		if len(last.Children) != 1 {
			return "", fmt.Errorf("entry function return statement has %d argument(s), want exactly one integer expression", len(last.Children))
		}
		returnExpr, err := buildExpr(unit, snapshot, last.Children[0], locals)
		if err != nil {
			return "", err
		}
		statements = append(statements, "    return "+returnExpr+";")
	case tir.If:
		tail, err := buildIfElseTail(unit, snapshot, last, locals)
		if err != nil {
			return "", err
		}
		statements = append(statements, tail)
	default:
		return "", fmt.Errorf("entry function body statement is a %s, want a Return of an integer expression or an if/else whose arms each return one", last.Kind)
	}
	return strings.Join(statements, "\n"), nil
}

// buildIfElseTail validates and builds the C text for the two-armed if/else
// this slice supports as an i32 entry's final statement: a tir.If with HasElse
// set, whose condition is a direct integer comparison (buildComparison) and
// whose two arms are Blocks each containing exactly one Return of one i32
// expression built via buildExpr with the same locals set the top-level body
// threads through. Locals declared earlier in the body are therefore visible
// in the condition and in both arms. The emitted text is the final if/else of
// pebble_user_main's body:
//
//	if (<condition>) {
//	    return <then expression>;
//	} else {
//	    return <else expression>;
//	}
//
// Any other shape — an If without an else, an arm that is not a Block with
// exactly one Return, an arm whose return has more than one argument — is a
// clean rejection naming what was found.
func buildIfElseTail(unit *tir.Unit, snapshot *types.Snapshot, ifNode tir.Node, locals map[symbol.SymbolID]bool) (string, error) {
	if !ifNode.HasElse {
		return "", fmt.Errorf("entry function body ends with an if without an else; this backend only supports the two-armed if/else whose arms each end in one return, found an if with no else")
	}
	if len(ifNode.Children) != 3 {
		return "", fmt.Errorf("entry function body ends with an if with %d child(ren), want exactly 3 (condition, then-arm, else-arm)", len(ifNode.Children))
	}
	condition, err := buildComparison(unit, snapshot, ifNode.Children[0], locals)
	if err != nil {
		return "", err
	}
	thenExpr, err := buildArmReturn(unit, snapshot, ifNode.Children[1], locals)
	if err != nil {
		return "", err
	}
	elseExpr, err := buildArmReturn(unit, snapshot, ifNode.Children[2], locals)
	if err != nil {
		return "", err
	}
	return fmt.Sprintf("    if (%s) {\n        return %s;\n    } else {\n        return %s;\n    }", condition, thenExpr, elseExpr), nil
}

// buildArmReturn validates one if/else arm and builds the C text of its single
// return. An arm must be a Block whose only child is a Return carrying exactly
// one i32 expression built via buildExpr; anything else is rejected. This is
// deliberately the same discipline buildIntegerEntryStatements applies to a
// body ending in one Return, narrowed to a single statement: locals declared
// inside an arm are not supported, and an arm's return must be a plain
// expression, not another if.
func buildArmReturn(unit *tir.Unit, snapshot *types.Snapshot, armID tir.NodeID, locals map[symbol.SymbolID]bool) (string, error) {
	arm, ok := unit.Node(armID)
	if !ok {
		return "", fmt.Errorf("entry function body if/else arm references invalid block node %d", armID)
	}
	if arm.Kind != tir.Block {
		return "", fmt.Errorf("entry function body if/else arm is a %s, want a Block containing exactly one Return of an integer expression", arm.Kind)
	}
	if len(arm.Children) != 1 {
		return "", fmt.Errorf("entry function body if/else arm contains %d statement(s), want exactly one Return of an integer expression", len(arm.Children))
	}
	ret, ok := unit.Node(arm.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body if/else arm references invalid statement node %d", arm.Children[0])
	}
	if ret.Kind != tir.Return {
		return "", fmt.Errorf("entry function body if/else arm contains a %s, want a Return of an integer expression", ret.Kind)
	}
	if len(ret.Children) != 1 {
		return "", fmt.Errorf("entry function body if/else arm return has %d argument(s), want exactly one integer expression", len(ret.Children))
	}
	return buildExpr(unit, snapshot, ret.Children[0], locals)
}

// buildComparison builds the C text for an if condition. It accepts exactly a
// tir.BinaryValue with two operands and one of the six comparison operators
// (<, <=, >, >=, ==, !=), and emits the plain C operator directly — comparing
// two integers cannot overflow, so no runtime helper is needed. Each operand
// is built by buildComparisonOperand (an int-typed integer literal, or any i32
// expression buildExpr accepts). Any other node kind, or any other operator on
// a BinaryValue (bitwise, and the && / || that lower to ShortCircuitValue
// nodes rather than BinaryValue comparisons), is a clean rejection.
func buildComparison(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]bool) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body if condition references invalid node %d", id)
	}
	if node.Kind != tir.BinaryValue {
		return "", fmt.Errorf("entry function body if condition is a %s, want a direct integer comparison (<, <=, >, >=, ==, or !=)", node.Kind)
	}
	if len(node.Children) != 2 {
		return "", fmt.Errorf("entry function body if condition has %d operand(s), want exactly two integer operands", len(node.Children))
	}
	op, ok := comparisonOperator(node.Operator)
	if !ok {
		return "", fmt.Errorf("entry function body if condition uses operator %s, want one of <, <=, >, >=, ==, or !=", node.Operator)
	}
	left, err := buildComparisonOperand(unit, snapshot, node.Children[0], locals)
	if err != nil {
		return "", err
	}
	right, err := buildComparisonOperand(unit, snapshot, node.Children[1], locals)
	if err != nil {
		return "", err
	}
	return left + " " + op + " " + right, nil
}

// buildComparisonOperand builds one comparison operand. A bare comparison
// between two untyped integer literals defaults both operands to the
// snapshot's int builtin (confirmed against a real fixture), so an
// IntegerLiteral of type int is lowered directly as its decimal text. Every
// other operand must be an i32 expression buildExpr accepts — a literal, a
// reference to a local declared earlier in the entry body, or checked negation
// and checked +, -, *, /, % arithmetic — and is delegated to buildExpr, whose
// own i32 gate and kind switch do the rejecting.
func buildComparisonOperand(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]bool) (string, error) {
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
	return buildExpr(unit, snapshot, id, locals)
}

// comparisonOperator maps the six comparison token kinds this backend lowers
// to their plain C spellings. These map 1:1 to C syntax — no runtime helper is
// involved, since comparing two i32 (or int) values cannot overflow. Any other
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

// buildExpr builds the C expression text for an i32 value node, recursing into
// its operands. locals is the set of symbols declared earlier in the entry
// body (a map is deliberately used, not a slice, so membership is a constant-
// time check); it is read-only for a SymbolValue reference and is otherwise
// threaded through unchanged. It accepts exactly four node kinds:
//
//   - IntegerLiteral — its decimal text (defensively validated, exactly as
//     10.3 validated a bare literal return).
//   - CheckedNegate with exactly one i32 operand — pebble_rt_checked_neg_i32.
//   - CheckedArithmetic with exactly two i32 operands and operator +, -, *, /,
//     or % — pebble_rt_checked_add_i32 / pebble_rt_checked_sub_i32 /
//     pebble_rt_checked_mul_i32 / pebble_rt_checked_div_i32 /
//     pebble_rt_checked_mod_i32.
//   - SymbolValue whose Symbol is in locals — pebble_local_<symbol ID>, the C
//     name buildIntegerEntryStatements gave that local's declaration.
//
// CheckedArithmetic with any other operator (the integral operators that build
// this node but are not yet lowered) is rejected, not guessed. A SymbolValue
// referencing anything not in locals (a global, a parameter, a symbol from an
// outer/different scope — none of which are reachable from this narrow body
// shape, but checked defensively rather than assumed) is a clean rejection.
// Any other node kind at any position — a function call, a non-i32 operand,
// CheckedShift, and so on — is a clean rejection naming what was found.
// Emitting the checked runtime helpers (rather than raw C operators) is what
// keeps the IR nodes' real overflow and divide-by-zero semantics from silently
// disappearing in the emitted program.
func buildExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]bool) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	if !isI32(snapshot, node.Type) {
		return "", fmt.Errorf("entry function body expression contains a %s of type %s, want i32", node.Kind, describeType(snapshot, node.Type))
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
		child, err := buildExpr(unit, snapshot, node.Children[0], locals)
		if err != nil {
			return "", err
		}
		return "pebble_rt_checked_neg_i32(" + child + ")", nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedArithmeticHelper(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedArithmetic with operator %s, want +, -, *, /, or %%", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, node.Children[0], locals)
		if err != nil {
			return "", err
		}
		right, err := buildExpr(unit, snapshot, node.Children[1], locals)
		if err != nil {
			return "", err
		}
		return helper + "(" + left + ", " + right + ")", nil
	case tir.SymbolValue:
		if !locals[node.Symbol] {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an integer literal, a reference to a local declared earlier in the body, or checked +, -, *, /, %% arithmetic", node.Kind)
	}
}

// checkedArithmeticHelper maps the +, -, *, /, % operators a CheckedArithmetic
// node may carry to the runtime helper that implements their checked semantics.
// Division and modulo map to pebble_rt_checked_div_i32 / pebble_rt_checked_mod_i32,
// which handle both the divide-by-zero fault (in every mode) and the one
// division overflow input, INT32_MIN / -1. Any other operator (bitwise, etc.)
// is deliberately not mapped and rejected by the caller.
func checkedArithmeticHelper(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Plus:
		return "pebble_rt_checked_add_i32", true
	case syntax.Minus:
		return "pebble_rt_checked_sub_i32", true
	case syntax.Star:
		return "pebble_rt_checked_mul_i32", true
	case syntax.Slash:
		return "pebble_rt_checked_div_i32", true
	case syntax.Percent:
		return "pebble_rt_checked_mod_i32", true
	default:
		return "", false
	}
}

// isI32 reports whether id is the snapshot's i32 builtin identity. The
// checked helpers this backend emits operate on i32 only, so every node in an
// accepted expression tree must carry exactly this type.
func isI32(snapshot *types.Snapshot, id types.TypeID) bool {
	return snapshot != nil && id == snapshot.Builtins().I32
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

// integerEntryUserMain is a format string; %s is the pre-indented statement
// sequence for pebble_user_main's body — zero or more `const int32_t
// pebble_local_<id> = <built init expression>;` declarations in declaration
// order, then the final `return <built return expression>;` — which becomes
// pebble_user_main's return value and, through the hosted main's own return,
// the process exit code. With no locals the sequence is exactly the single
// return statement, so the zero-locals shape emits byte-identically to before.
const integerEntryUserMain = `static int pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
%s
}`

const integerEntryMainBody = `return pebble_user_main(&ctx);`

// emitEntryC writes the shared adapter skeleton once the typed IR has been
// confirmed to describe one of the two supported program shapes.
func emitEntryC(w io.Writer, userMain, mainBody string) error {
	_, err := fmt.Fprintf(w, `#include "pebble_rt.h"

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
