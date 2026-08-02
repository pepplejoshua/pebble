// Package backend lowers typed IR to C source emitted against the versioned
// runtime ABI (runtime/include/pebble_rt.h). It is deliberately narrow: the
// current slice emits exactly two program shapes — an empty-bodied Pebble-
// convention void entry function, and a zero-parameter i32 entry whose body
// is exactly one `return <i32 expression>;` where the expression is a small
// tree of integer literals, checked negation, and checked +, -, * arithmetic —
// and rejects everything else with a descriptive error instead of guessing.
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
// {}` produces) or i32 with a body of exactly one `return <i32 expression>;`
// statement, in which case the expression's value is propagated as the
// process's exit code. The expression may be a plain non-negative integer
// literal, or a tree of checked negation and checked +, -, * arithmetic (see
// buildExpr) — checked operations emit pebble_rt_checked_*_i32 calls so the
// language's overflow semantics survive into the emitted program. Any other
// shape returns a descriptive error and writes nothing to w; this package does
// not yet lower arbitrary expressions or statements.
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
	expr, err := buildReturnExpression(unit, snapshot, block)
	if err != nil {
		return err
	}
	return emitEntryC(w, fmt.Sprintf(integerEntryUserMain, expr), integerEntryMainBody)
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

// buildReturnExpression validates the i32 entry's supported body and builds the
// C text for its return value. The accepted body: a block with exactly one
// statement, a Return carrying exactly one argument, and a value node that
// buildExpr can lower (a plain non-negative integer literal, or a tree of
// checked negation and checked +, -, * arithmetic). Any other shape is
// rejected with a descriptive error, not best-effort lowered.
func buildReturnExpression(unit *tir.Unit, snapshot *types.Snapshot, block tir.Node) (string, error) {
	if len(block.Children) != 1 {
		return "", fmt.Errorf("entry function body has %d statement(s), want exactly one return of an integer expression", len(block.Children))
	}
	ret, ok := unit.Node(block.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body references invalid statement node %d", block.Children[0])
	}
	if ret.Kind != tir.Return {
		return "", fmt.Errorf("entry function body statement is a %s, want a Return of an integer expression", ret.Kind)
	}
	if len(ret.Children) != 1 {
		return "", fmt.Errorf("entry function return statement has %d argument(s), want exactly one integer expression", len(ret.Children))
	}
	return buildExpr(unit, snapshot, ret.Children[0])
}

// buildExpr builds the C expression text for an i32 value node, recursing into
// its operands. It accepts exactly three node kinds:
//
//   - IntegerLiteral — its decimal text (defensively validated, exactly as
//     10.3 validated a bare literal return).
//   - CheckedNegate with exactly one i32 operand — pebble_rt_checked_neg_i32.
//   - CheckedArithmetic with exactly two i32 operands and operator +, -, or *
//     — pebble_rt_checked_add_i32 / pebble_rt_checked_sub_i32 /
//     pebble_rt_checked_mul_i32.
//
// CheckedArithmetic with any other operator (division, modulo, and the other
// integral operators that build this node) is rejected, not guessed: division
// and modulo need a different fault category (divide-by-zero), which is out of
// scope for this slice. Any other node kind at any position — a variable
// reference (SymbolValue), a function call, a non-i32 operand, CheckedShift,
// and so on — is a clean rejection naming what was found. Emitting the checked
// runtime helpers (rather than raw C + - *) is what keeps the IR nodes' real
// overflow semantics from silently disappearing in the emitted program.
func buildExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function return expression references invalid node %d", id)
	}
	if !isI32(snapshot, node.Type) {
		return "", fmt.Errorf("entry function return expression contains a %s of type %s, want i32", node.Kind, describeType(snapshot, node.Type))
	}
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("entry function return expression contains an integer literal with malformed text %q", text)
		}
		return text, nil
	case tir.CheckedNegate:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function return expression contains a CheckedNegate with %d operand(s), want exactly one", len(node.Children))
		}
		if node.Operator != syntax.Minus {
			return "", fmt.Errorf("entry function return expression contains a CheckedNegate with operator %s, want -", node.Operator)
		}
		child, err := buildExpr(unit, snapshot, node.Children[0])
		if err != nil {
			return "", err
		}
		return "pebble_rt_checked_neg_i32(" + child + ")", nil
	case tir.CheckedArithmetic:
		if len(node.Children) != 2 {
			return "", fmt.Errorf("entry function return expression contains a CheckedArithmetic with %d operand(s), want exactly two", len(node.Children))
		}
		helper, ok := checkedArithmeticHelper(node.Operator)
		if !ok {
			return "", fmt.Errorf("entry function return expression contains a CheckedArithmetic with operator %s, want +, -, or * (division and modulo need a different fault category, not supported yet)", node.Operator)
		}
		left, err := buildExpr(unit, snapshot, node.Children[0])
		if err != nil {
			return "", err
		}
		right, err := buildExpr(unit, snapshot, node.Children[1])
		if err != nil {
			return "", err
		}
		return helper + "(" + left + ", " + right + ")", nil
	default:
		return "", fmt.Errorf("entry function return expression contains a %s, want an integer literal or checked +, -, * arithmetic", node.Kind)
	}
}

// checkedArithmeticHelper maps the +, -, * operators a CheckedArithmetic node
// may carry to the runtime helper that implements their checked overflow
// semantics. Any other operator (division, modulo, bitwise) is deliberately
// not mapped: division and modulo fault on divide-by-zero, a different
// category this slice does not yet emit.
func checkedArithmeticHelper(op syntax.TokenKind) (string, bool) {
	switch op {
	case syntax.Plus:
		return "pebble_rt_checked_add_i32", true
	case syntax.Minus:
		return "pebble_rt_checked_sub_i32", true
	case syntax.Star:
		return "pebble_rt_checked_mul_i32", true
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

// integerEntryUserMain is a format string; %s is the built return expression
// text (a plain integer literal, or a chain of pebble_rt_checked_*_i32 calls),
// which becomes pebble_user_main's return value and, through the hosted main's
// own return, the process exit code.
const integerEntryUserMain = `static int pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
    return %s;
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
