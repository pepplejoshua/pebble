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
//
// Since 10.17, an integer entry's body may also call other Pebble-convention
// functions declared in the same unit. Only functions actually
// reachable from the entry by a tir.DirectCall are emitted (discoverReachableHelpers
// does a post-order worklist walk starting from the entry's body, following
// every call), each as its own `static <width> pebble_fn_<symbolID>(PebbleContext
// *ctx, <params>...)` block emitted before pebble_user_main so a called
// function's C definition precedes its use. A called function's parameters
// (each of the entry's resolved width or bool) seed its own locals scope
// before its body is built and are declared in the C signature with the same
// pebble_local_<symbolID> naming locals use, so a reference to a parameter
// inside the body is read exactly like a reference to a declared local.
// Every called function must resolve to the
// entry's own integer width — there is no cast/coercion lowering, and
// void-result helpers are deliberately out of scope this slice (a void call
// has no expression-statement construct in the block grammar). Recursion
// (self- or mutual) is rejected cleanly at discovery time, since this backend
// has no forward-declaration mechanism yet. Each called function's body is
// built by the exact same buildBlock, with its own fresh locals scope seeded
// with the function's own parameters.
// A call expression emits `pebble_fn_<calleeSymbolID>(ctx, <arg0>, <arg1>,
// ...)`: the typed IR's
// DirectCall records context forwarding via ContextAction (ContextForward for a
// Pebble-convention call) but carries no explicit context argument, so the
// backend prepends ctx itself, exactly as pebble_user_main receives it, and
// each argument is built by the grammar its callee parameter resolves to.
//
// Since 10.19, a local may also be declared as a tuple whose element types are
// exactly the entry's resolved width and/or bool, initialized from a tuple
// literal (a tir.TupleValue), with individual elements read back as ordinary
// values (a tir.Load of a tir.TuplePlace — the only shape real source produces
// for reading a tuple-typed local's element; see buildExpr's Load case). A
// tuple type is emitted as one C struct typedef, named pebble_tuple_<typeID>_t
// from the tuple type's own stable types.TypeID and written once per distinct
// tuple type before any function that references it (C requires definition
// before use); each typedef's fields are the positional `_0`, `_1`, ... in
// element order. Tuple element types are restricted to the entry's width and
// bool, reads route through buildExpr/buildBoolExpr by the element's own type,
// and every other tuple shape — a nested tuple element, a str element, a whole
// tuple copied from another value, assigning into or reassigning a tuple
// local, a tuple parameter or result, a TupleElementValue indexing a tuple
// literal, or a TupleCoerce — is a clean rejection, never a guessed lowering.
//
// Since 10.21, a local may also be declared as an optional whose payload type
// is exactly the entry's resolved width (i32 or i64) or bool, initialized from
// a `some <expr>` expression (a tir.SomeOptional), and force-unwrapped with
// the postfix `!` operator (a tir.CheckedOptionalUnwrap). An optional type is
// emitted as one C struct typedef, named pebble_optional_<typeID>_t from the
// optional type's own stable types.TypeID, with two fields: `bool has_value`
// and `value` (the payload's C type). Force-unwrap is bounds-checked via the
// runtime helper pebble_rt_checked_unwrap_i32/i64/bool. `none` literals
// (a tir.NoneOptional) construct an absent optional the same way; reassigning
// an optional local is rejected cleanly. Optional-typed
// function parameters, return types, and payload types other than the entry's
// width or bool are out of scope and rejected cleanly.
//
// Since 10.22, a local may also be declared as a struct whose every field's
// type is exactly the entry's resolved width or bool, initialized from a
// struct literal (a tir.RecordConstruct, e.g. Point.{ x = 1, y = 2 }), with
// individual fields read back as ordinary values (a tir.Load of a
// tir.FieldPlace — the only shape real source produces for reading a
// struct-typed local's field; see buildExpr's Load case). A struct type is
// emitted as one C struct typedef, named pebble_struct_<typeID>_t from the
// struct type's own stable types.TypeID and written once per distinct struct
// type before any function that references it; each typedef's fields are
// named pebble_field_<memberSymbolID> from each field's own stable
// symbol.SymbolID, in the struct's declared field order (from its
// TypeDeclaration's member list — a construction site may list fields in any
// order, so the declared order is resolved by the collection pass, not taken
// from any RecordConstruct). A struct local's initializer is a C99
// designated-initializer brace list ({ .pebble_field_25 = 1, ... }), which
// sidesteps the construction-vs-declared field ordering problem entirely.
// Field reads route through buildExpr/buildBoolExpr by the field's own type,
// and every other struct shape — a struct field of any type other than the
// entry's width or bool, a whole struct copied from another value, assigning
// into or reassigning a struct local, a struct parameter or result, a
// FieldValue reading a field off a struct literal directly, or nested field
// access — is a clean rejection, never a guessed lowering.
//
// Since 10.23, a local may also be declared as a str value, initialized from a
// string literal (a tir.StringLiteral) only. A str local is declared directly
// as the runtime ABI's PebbleStr (runtime/include/pebble_rt.h) — a fixed
// runtime type, not a program-specific shape — initialized from the literal's
// decoded bytes re-escaped into a safe C string literal (escapeCString emits a
// fixed-width octal escape for every non-printable byte, so C's maximal-munch
// escape rules can never swallow a following digit) and its compile-time
// decoded byte length, so no runtime strlen is involved. Two str values may be
// compared with ==/!= — each operand either a str-typed local (a SymbolValue,
// built by buildStrOperand) or another string literal directly — emitting the
// runtime helper pebble_rt_str_eq(<a>, <b>) (==) or its negation (!=); a str
// comparison lowers to a plain tir.BinaryValue with two un-wrapped operand
// nodes (confirmed against a real fixture), handled in buildComparison
// alongside the integer and bool comparison paths. Everything else str-shaped
// is out of scope and a clean rejection: reassigning a str local, str-typed
// function parameters/results, str fields/elements inside a tuple, array,
// optional, or struct, ordering comparisons between strs (reachable from real
// source but rejected), concatenation and interpolation (InterpolatedString),
// and str indexing (a tir.CheckedIndex, reachable from real source via e.g.
// `let c char = s[0];` — a separate mechanism this backend does not build for
// str, rejected because its char result is not a supported local type).
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
// checked +, -, *, /, % arithmetic (see buildExpr), a reference to a local
// declared earlier in the same or an enclosing block, or a call to another
// Pebble-convention zero-parameter function whose result is the entry's own
// width (a tir.DirectCall, see buildExpr). A comparison's operands
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
//
// An integer entry may additionally call other functions. Every function
// actually reachable from the entry by a call (transitively — the reachability
// walk follows into each called function's own body) is validated and emitted
// as its own static helper function before pebble_user_main, with each called
// function's body built by the same buildBlock against a fresh locals scope
// seeded with that function's own parameters
// (see discoverReachableHelpers and buildHelperFunctions). A called function
// must be Pebble-convention, take parameters of only the entry's resolved
// width or bool, and return exactly the
// entry's resolved width; a width mismatch at a call site, a parameter of any
// other type, a void-result
// helper (deliberately out of scope this slice), or a call that is part of a
// cycle (a function that can reach itself, directly or through others — the
// recursion boundary) is a clean rejection naming what was found, since this
// backend has no forward-declaration mechanism to order recursive or
// out-of-definition-order calls yet.
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
		return emitEntryC(w, "", "", voidEntryUserMain, voidEntryMainBody)
	}
	helpers, err := discoverReachableHelpers(unit, snapshot, decl, blockID, result)
	if err != nil {
		return err
	}
	tupleTypes, err := collectTupleTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	optionalTypes, err := collectOptionalTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	structInfos, err := collectStructTypes(unit, snapshot, blockID, helpers)
	if err != nil {
		return err
	}
	tupleTypedefs, err := buildTupleTypedefs(snapshot, result, tupleTypes)
	if err != nil {
		return err
	}
	optionalTypedefs, err := buildOptionalTypedefs(snapshot, result, optionalTypes)
	if err != nil {
		return err
	}
	structTypedefs, err := buildStructTypedefs(snapshot, result, structInfos)
	if err != nil {
		return err
	}
	typedefs := joinTypedefs(tupleTypedefs, joinTypedefs(optionalTypedefs, structTypedefs))
	helpersText, err := buildHelperFunctions(unit, snapshot, helpers, result)
	if err != nil {
		return err
	}
	statements, err := buildBlock(unit, snapshot, blockID, nil, 0, result)
	if err != nil {
		return err
	}
	return emitEntryC(w, typedefs, helpersText, fmt.Sprintf(integerEntryUserMain, entryReturnType(result), statements), integerEntryMainBody)
}

// findEntryDeclaration locates the FunctionDeclaration node for entrySymbol.
// A specialization would carry non-empty TypeArgs; the entry cannot be
// generic, so those are deliberately excluded rather than assumed absent.
func findEntryDeclaration(unit *tir.Unit, entrySymbol symbol.SymbolID) (tir.Node, error) {
	return findFunctionDeclaration(unit, entrySymbol, "entry function")
}

// findFunctionDeclaration locates the non-generic FunctionDeclaration node
// for the given function symbol, generalizing findEntryDeclaration to any
// function the reachability walk resolves. Every typed-IR function this
// backend emits — the entry and every called helper — has exactly one such
// declaration; a generic instance would carry non-empty TypeArgs and is
// excluded, since generic calls are not lowered here.
func findFunctionDeclaration(unit *tir.Unit, symbolID symbol.SymbolID, what string) (tir.Node, error) {
	for _, node := range unit.Nodes() {
		if node.Kind != tir.FunctionDeclaration || node.Symbol != symbolID || len(node.TypeArgs) != 0 {
			continue
		}
		return node, nil
	}
	return tir.Node{}, fmt.Errorf("%s not found in unit: no non-generic FunctionDeclaration for symbol %d", what, symbolID)
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
	return findFunctionBody(unit, decl, "entry function")
}

// findFunctionBody resolves the body Block for any function declaration,
// generalizing findEntryBody to a called helper: it follows the declaration's
// FunctionID to its FunctionDecl container and resolves that container's body
// node, returning both the Block node and its NodeID.
func findFunctionBody(unit *tir.Unit, decl tir.Node, what string) (tir.Node, tir.NodeID, error) {
	for _, fd := range unit.FunctionDeclarations() {
		if fd.FunctionID != decl.Function {
			continue
		}
		block, ok := unit.Node(fd.Node)
		if !ok {
			return tir.Node{}, 0, fmt.Errorf("%s body not found in unit: FunctionDecl %d has invalid body node %d", what, fd.FunctionID, fd.Node)
		}
		if block.Kind != tir.Block {
			return tir.Node{}, 0, fmt.Errorf("%s body is a %s, want a Block", what, block.Kind)
		}
		return block, fd.Node, nil
	}
	return tir.Node{}, 0, fmt.Errorf("%s body declaration not found in unit: no FunctionDecl for FunctionID %d", what, decl.Function)
}

// helperInfo is one reachable non-entry function discovered by
// discoverReachableHelpers: its FunctionDeclaration node (for validation and
// for the deterministic pebble_fn_<symbolID> C name) and the NodeID of its
// body Block (for buildBlock). The emission order of the returned slice is a
// post-order of the reachability walk — every callee precedes its caller — so
// a called function's C definition always precedes its use in the generated
// file.
type helperInfo struct {
	decl  tir.Node
	block tir.NodeID
}

// reachabilityWalk carries the mutable state of the recursive reachability
// discovery in discoverReachableHelpers: the functions already fully walked
// (done), the functions on the current DFS path (stack — a callee found on
// the path is a cycle), and the post-order emission list (order).
type reachabilityWalk struct {
	unit     *tir.Unit
	snapshot *types.Snapshot
	width    types.BuiltinKind
	entry    symbol.SymbolID
	done     map[symbol.SymbolID]bool
	stack    []symbol.SymbolID
	order    []helperInfo
}

// discoverReachableHelpers finds exactly the set of non-entry functions the
// entry actually calls, transitively, by walking the entry's body for
// tir.DirectCall nodes and recursing into each newly-discovered callee's own
// body — a worklist/DFS over the direct-call edges starting from the entry,
// following into every function reached. Emitting exactly this reachable set
// (and nothing else) guarantees by construction that every emitted helper has
// at least one call site, so the mandated -Wall -Wextra -Werror build never
// warns about an unused static function. Each reached callee is validated
// (Pebble-convention, parameters each of the entry's width or bool, result
// exactly the entry's width —
// validateHelperSignature) and its body located (findFunctionBody) before
// recursing. The returned slice is a post-order of the walk — callees before
// callers — which is the emission order that keeps every call in the emitted
// C text forward (definition before use), since this backend has no
// forward-declaration mechanism. A cycle (a function that can reach itself,
// directly or through others) is a clean rejection naming the cycle, not
// attempted.
func discoverReachableHelpers(unit *tir.Unit, snapshot *types.Snapshot, entryDecl tir.Node, entryBlockID tir.NodeID, width types.BuiltinKind) ([]helperInfo, error) {
	walk := &reachabilityWalk{
		unit:     unit,
		snapshot: snapshot,
		width:    width,
		entry:    entryDecl.Symbol,
		done:     make(map[symbol.SymbolID]bool),
	}
	if err := walk.visit(entryDecl, entryBlockID); err != nil {
		return nil, err
	}
	return walk.order, nil
}

// visit walks one function's body for DirectCall nodes, recursing into every
// discovered callee's own body. The entry is the root of the walk; the entry
// itself is never added to the emission order (its C definition,
// pebble_user_main, is emitted separately after the helpers). A callee
// already fully walked (done) is a shared subgraph — a diamond, where two
// callers reach one callee — and is skipped, not re-walked, so each helper is
// emitted exactly once. A callee already on the current DFS path (stack) is a
// cycle and is rejected, naming the call chain that closes on itself.
func (w *reachabilityWalk) visit(decl tir.Node, blockID tir.NodeID) error {
	if w.done[decl.Symbol] {
		return nil
	}
	if inStack := indexOfSymbol(w.stack, decl.Symbol); inStack >= 0 {
		// The function is already on the current DFS path, so the call edge
		// just followed closes a cycle: decl can reach itself through
		// stack[inStack:] -> decl. Forward-declaration ordering for recursive
		// calls is real future work, not this slice's problem.
		cycle := append(append([]symbol.SymbolID(nil), w.stack[inStack:]...), decl.Symbol)
		parts := make([]string, len(cycle))
		for i, id := range cycle {
			parts[i] = fmt.Sprintf("symbol %d", id)
		}
		return fmt.Errorf("recursion is not supported yet: the call chain %s is a cycle (a function that can reach itself, directly or through others), and this backend has no forward-declaration mechanism to order recursive calls yet", strings.Join(parts, " -> "))
	}
	w.stack = append(w.stack, decl.Symbol)
	var calls []tir.Node
	if err := collectDirectCalls(w.unit, blockID, &calls); err != nil {
		return err
	}
	for _, call := range calls {
		if len(call.TypeArgs) != 0 {
			return fmt.Errorf("called function symbol %d is a generic call with %d type argument(s), which this backend does not lower (generics are not supported yet)", call.Symbol, len(call.TypeArgs))
		}
		calleeDecl, err := findFunctionDeclaration(w.unit, call.Symbol, "called function")
		if err != nil {
			return err
		}
		if err := validateHelperSignature(calleeDecl, w.snapshot, w.width); err != nil {
			return err
		}
		_, calleeBlock, err := findFunctionBody(w.unit, calleeDecl, "called function")
		if err != nil {
			return err
		}
		if err := w.visit(calleeDecl, calleeBlock); err != nil {
			return err
		}
	}
	w.stack = w.stack[:len(w.stack)-1]
	w.done[decl.Symbol] = true
	if decl.Symbol != w.entry {
		w.order = append(w.order, helperInfo{decl: decl, block: blockID})
	}
	return nil
}

// collectDirectCalls appends every tir.DirectCall node in the tree rooted at
// nodeID, following Children and DeferChain. The typed-IR node graph is
// single-parented, so this walk terminates and each node is visited at most
// once per path. DeferChain is followed for completeness even though defer is
// rejected by the block builders anyway — following it only affects which
// callees are validated, never whether the program is accepted (a deferring
// body is rejected on its own merits).
func collectDirectCalls(unit *tir.Unit, nodeID tir.NodeID, out *[]tir.Node) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("reachability walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.DirectCall {
		*out = append(*out, node)
	}
	for _, childID := range node.Children {
		if err := collectDirectCalls(unit, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectDirectCalls(unit, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectTupleTypes appends, in first-encountered order, the tuple TypeID of
// every tuple type the emitted program actually references: the entry body
// (root) followed by every reachable helper's body, each walked by the same
// Children + DeferChain traversal collectDirectCalls uses. A tuple type is
// referenced in exactly two places in the emitted C — a tuple-typed local's
// declaration (an Initialize whose initializer value carries the tuple type)
// and a tuple construction (a TupleValue, whose Type is the tuple type) — so
// collecting exactly those two node shapes guarantees every typedef the
// program needs is discovered. The caller deduplicates (see Emit) so each
// distinct tuple type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectTupleTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectTupleTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectTupleTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectTupleTypesWalk appends every tuple type encountered in the tree rooted
// at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry a
// tuple type: a TupleValue node's own Type, and an Initialize whose initializer
// value carries a tuple type (a tuple-typed local declaration — the local's
// type is recorded on the initializer value node, not on the Initialize node
// itself, confirmed against a real fixture). A tuple initializer that is not a
// TupleValue (a whole-tuple copy of another local) is still a tuple-typed
// local and still needs its typedef; it is collected here by the Initialize
// rule even though buildLeadingStatement rejects that initializer shape.
func collectTupleTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("tuple-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.TupleValue {
		*out = append(*out, node.Type)
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isTuple(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectTupleTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectTupleTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// collectOptionalTypes appends, in first-encountered order, the optional
// TypeID of every optional type the emitted program actually references: the
// entry body (root) followed by every reachable helper's body, each walked
// by the same Children + DeferChain traversal collectDirectCalls uses. An
// optional type is referenced in exactly two places in the emitted C — an
// optional-typed local's declaration (an Initialize whose initializer value
// carries the optional type) and a SomeOptional node (whose Type is the
// optional type) — so collecting exactly those two node shapes guarantees
// every typedef the program needs is discovered. The caller deduplicates so
// each distinct optional type yields exactly one typedef, emitted before any
// function definition in the final output.
func collectOptionalTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]types.TypeID, error) {
	var collected []types.TypeID
	if err := collectOptionalTypesWalk(unit, snapshot, entryBlockID, &collected); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectOptionalTypesWalk(unit, snapshot, helper.block, &collected); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var deduplicated []types.TypeID
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		deduplicated = append(deduplicated, id)
	}
	return deduplicated, nil
}

// collectOptionalTypesWalk appends every optional type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry an
// optional type: a SomeOptional node's own Type, and an Initialize whose
// initializer value carries an optional type (an optional-typed local
// declaration — confirmed against a real fixture: the local's type is recorded
// on the initializer value node, not on the Initialize node itself). The
// Initialize rule alone covers a `none`-initialized local too (a NoneOptional
// node carries its own optional Type exactly like SomeOptional does), so no
// separate NoneOptional case is needed here.
func collectOptionalTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("optional-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.SomeOptional {
		if isOptional(snapshot, node.Type) {
			*out = append(*out, node.Type)
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isOptional(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectOptionalTypesWalk(unit, snapshot, childID, out); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectOptionalTypesWalk(unit, snapshot, deferID, out); err != nil {
			return err
		}
	}
	return nil
}

// structFieldInfo is one field of a distinct struct type the emitted program
// references, resolved from the struct's own declaration: its member symbol
// (the field's stable symbol.SymbolID, the basis of the C field name
// pebble_field_<member>) and the field's resolved types.TypeID.
type structFieldInfo struct {
	member symbol.SymbolID
	typ    types.TypeID
}

// structInfo is one distinct struct type the emitted program references,
// carrying everything buildStructTypedef needs: the struct's own types.TypeID
// (the basis of the C typedef name pebble_struct_<typeID>_t), its declaration
// symbol, and its fields in the struct's *declared* order (from the struct's
// own TypeDecl.Members — a RecordConstruct's Fields may list the fields in
// any construction-site order, confirmed against a real fixture, so the
// declared order is resolved here once rather than at emit time).
type structInfo struct {
	typ    types.TypeID
	decl   symbol.SymbolID
	fields []structFieldInfo
}

// collectStructTypes resolves, in first-encountered order, every struct type
// the emitted program actually references: the entry body (root) followed by
// every reachable helper's body, each walked by the same Children + DeferChain
// traversal collectDirectCalls uses. A struct type is referenced in exactly
// two places in the emitted C — a struct-typed local's declaration (an
// Initialize whose initializer value carries the struct type) and a struct
// construction (a RecordConstruct, whose Type is the struct type) — so
// collecting exactly those two node shapes guarantees every typedef the
// program needs is discovered. The walk also accumulates each field's resolved
// type from the same nodes (a RecordConstruct field value's own type, and a
// FieldPlace's Type), since the FieldDeclaration nodes in the unit carry only
// the field's symbol, never its type (confirmed against a real fixture — a
// further lookup is required, the same kind of confirmation 10.18 did for
// FunctionDeclaration.Parameters). The returned structInfos are deduplicated
// by struct TypeID and each resolved to its declared field order, so every
// distinct struct type yields exactly one typedef, emitted before any function
// definition in the final output.
func collectStructTypes(unit *tir.Unit, snapshot *types.Snapshot, entryBlockID tir.NodeID, helpers []helperInfo) ([]structInfo, error) {
	fieldTypes := make(map[symbol.SymbolID]types.TypeID)
	var collected []types.TypeID
	if err := collectStructTypesWalk(unit, snapshot, entryBlockID, &collected, fieldTypes); err != nil {
		return nil, err
	}
	for _, helper := range helpers {
		if err := collectStructTypesWalk(unit, snapshot, helper.block, &collected, fieldTypes); err != nil {
			return nil, err
		}
	}
	seen := make(map[types.TypeID]bool, len(collected))
	var infos []structInfo
	for _, id := range collected {
		if seen[id] {
			continue
		}
		seen[id] = true
		info, err := resolveStructInfo(unit, snapshot, id, fieldTypes)
		if err != nil {
			return nil, err
		}
		infos = append(infos, info)
	}
	return infos, nil
}

// collectStructTypesWalk appends every struct type encountered in the tree
// rooted at nodeID to out, in first-encountered order, following Children and
// DeferChain exactly like collectDirectCalls so it visits the same reachable
// region of the node graph the body builders consume. Two node shapes carry a
// struct type: a RecordConstruct node's own Type, and an Initialize whose
// initializer value carries a struct type (a struct-typed local declaration —
// the local's type is recorded on the initializer value node, not on the
// Initialize node itself, confirmed against a real fixture, the same finding
// tuple/array/optional collection made). The same walk also records, in
// fieldTypes, every field symbol's resolved type from exactly the two nodes
// that carry it: a RecordConstruct field value node's own Type, and a
// FieldPlace node's Type — the only in-unit sources of a field's type, since
// the FieldDeclaration node carries no type.
func collectStructTypesWalk(unit *tir.Unit, snapshot *types.Snapshot, nodeID tir.NodeID, out *[]types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) error {
	node, ok := unit.Node(nodeID)
	if !ok {
		return fmt.Errorf("struct-type walk references invalid node %d", nodeID)
	}
	if node.Kind == tir.RecordConstruct {
		*out = append(*out, node.Type)
		for _, field := range node.Fields {
			if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
				fieldTypes[field.Field] = value.Type
			}
		}
	}
	if node.Kind == tir.FieldPlace {
		if node.Member != 0 && node.Type != 0 {
			fieldTypes[node.Member] = node.Type
		}
	}
	if node.Kind == tir.Initialize {
		for _, childID := range node.Children {
			if child, ok := unit.Node(childID); ok && isStruct(snapshot, child.Type) {
				*out = append(*out, child.Type)
			}
		}
	}
	for _, childID := range node.Children {
		if err := collectStructTypesWalk(unit, snapshot, childID, out, fieldTypes); err != nil {
			return err
		}
	}
	for _, deferID := range node.DeferChain {
		if err := collectStructTypesWalk(unit, snapshot, deferID, out, fieldTypes); err != nil {
			return err
		}
	}
	return nil
}

// resolveStructInfo turns one collected struct TypeID into a structInfo with
// its fields in declared order. The declaration symbol comes from the type's
// own Nominal key (TypeKey.Nominal); the declared field order comes from the
// corresponding TypeDecl's Members (unit.TypeDeclarations), which lists the
// field symbols in the struct's source declaration order — NOT the
// construction-site order a RecordConstruct's Fields carry, which is why the
// order is resolved here rather than from any construction node. Each field's
// type comes from the fieldTypes map accumulated by the walk; a member with no
// recorded type is a clean rejection, not a guessed layout.
func resolveStructInfo(unit *tir.Unit, snapshot *types.Snapshot, id types.TypeID, fieldTypes map[symbol.SymbolID]types.TypeID) (structInfo, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Nominal {
		return structInfo{}, fmt.Errorf("type %s is a %v, want a struct type", structTypeName(id), key.Kind())
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return structInfo{}, fmt.Errorf("type %s has no nominal declaration", structTypeName(id))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return structInfo{}, fmt.Errorf("struct type %s has no TypeDeclaration for symbol %d in the unit", structTypeName(id), decl)
	}
	fields := make([]structFieldInfo, len(typeDecl.Members))
	for i, member := range typeDecl.Members {
		fieldType, ok := fieldTypes[member]
		if !ok {
			return structInfo{}, fmt.Errorf("struct type %s field symbol %d has no resolvable type in the unit", structTypeName(id), member)
		}
		fields[i] = structFieldInfo{member: member, typ: fieldType}
	}
	return structInfo{typ: id, decl: decl, fields: fields}, nil
}

// findTypeDeclaration locates the TypeDecl container (its ordered Members list
// names the struct's declared fields) for a type declaration symbol. The
// TypeDeclaration *node* in the unit carries only the Symbol — its field list
// is on the TypeDecl container the builder published alongside it (the same
// division the unit makes between FunctionDeclaration nodes and FunctionDecl
// containers), so the container is the authoritative declared-field-order
// source.
func findTypeDeclaration(unit *tir.Unit, symbolID symbol.SymbolID) (tir.TypeDecl, bool) {
	for _, td := range unit.TypeDeclarations() {
		if td.Symbol == symbolID {
			return td, true
		}
	}
	return tir.TypeDecl{}, false
}

// indexOfSymbol returns the position of id in ids, or -1 if absent.
func indexOfSymbol(ids []symbol.SymbolID, id symbol.SymbolID) int {
	for i, candidate := range ids {
		if candidate == id {
			return i
		}
	}
	return -1
}

// validateHelperSignature checks one called function against the constraints
// every reachable helper must satisfy: Pebble-convention, parameters whose
// types are exactly the entry's resolved width or bool, and a result of
// exactly the entry's resolved width. The width rule is the same reasoning
// 10.13 established for locals — a called function of the other width (an i32
// helper called from an i64 entry, or vice versa) is a clean width-mismatch
// rejection, never a coercion, since there is no cast/coercion lowering to
// fall back on. A parameter's own type has the same two options a local has:
// the entry's width, or bool — anything else (str, a pointer, an array, a
// helper of the other integer width) is a clean rejection naming the position.
// A void-result helper is also a
// clean rejection: this slice only supports integer-result calls used as
// expression values, deliberately leaving bare void calls (which would need an
// expression-statement construct in the block grammar) out of scope.
func validateHelperSignature(decl tir.Node, snapshot *types.Snapshot, width types.BuiltinKind) error {
	if decl.Convention != types.Pebble {
		return fmt.Errorf("called function symbol %d uses %s calling convention, want Pebble", decl.Symbol, callingConventionName(decl.Convention))
	}
	for i, param := range decl.Parameters {
		// A parameter's type is resolved the same way a local's initializer's
		// is: the entry's resolved width (built by buildExpr) or bool (built by
		// buildBoolExpr), nothing else. This is exactly the width-consistency
		// rule 10.13 established for locals, applied to parameters.
		if !isWidth(snapshot, width, param.Type) && !isBool(snapshot, param.Type) {
			return fmt.Errorf("called function symbol %d parameter %d (symbol %d) has type %s, want %s or bool (a parameter may only be the entry's integer width or bool)", decl.Symbol, i, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
	}
	if !isWidth(snapshot, width, decl.ResultType) {
		if builtin, ok := resolvedBuiltin(snapshot, decl.ResultType); ok && builtin == types.Void {
			return fmt.Errorf("called function symbol %d returns void; void-result helper calls are not supported yet (only %s-result calls used as expression values are)", decl.Symbol, wantName(width))
		}
		return fmt.Errorf("called function symbol %d has result type %s, want %s (a called function must resolve to the entry's integer width)", decl.Symbol, describeType(snapshot, decl.ResultType), wantName(width))
	}
	return nil
}

// buildHelperFunctions builds the C text for every reachable helper, in the
// post-order discovery gives (callees before callers), each as its own
// `static <width> pebble_fn_<symbolID>(PebbleContext *ctx, <params>...) { ... }`
// block with its body built by the exact same buildBlock the entry's body
// uses — no parallel body-builder. Before the body is built, the helper's own
// parameters seed its locals scope exactly as if each had been Initialize'd:
// every parameter maps to its resolved type (the entry's width or bool), so a
// SymbolValue reference or a Store targeting a parameter inside the body
// resolves through the existing machinery unchanged. The C signature declares
// each parameter with the same pebble_local_<symbolID> naming every local
// uses, so a parameter and a local are textually identical inside the body
// (which is correct: they behave identically once inside the function). Each
// parameter also gets a `(void)pebble_local_<symbolID>;` immediately after
// the opening brace, the same -Wunused-parameter defense the `(void)ctx;`
// already provides for the context (confirmed: -Wunused-parameter genuinely
// fires under -Wall -Wextra -Werror for a declared-but-never-read parameter).
// Each helper gets its own fresh scope for anything its body declares (the
// seeded parameters plus whatever buildBlock adds), so a helper's locals are
// invisible to the entry and to sibling helpers, exactly as two blocks at the
// same nesting level are isolated.
func buildHelperFunctions(unit *tir.Unit, snapshot *types.Snapshot, helpers []helperInfo, width types.BuiltinKind) (string, error) {
	texts := make([]string, 0, len(helpers))
	for _, helper := range helpers {
		scope := make(map[symbol.SymbolID]localInfo, len(helper.decl.Parameters))
		params := make([]string, 0, len(helper.decl.Parameters))
		casts := make([]string, 0, len(helper.decl.Parameters))
		for _, param := range helper.decl.Parameters {
			kind, ok := resolvedBuiltin(snapshot, param.Type)
			if !ok {
				return "", fmt.Errorf("called function symbol %d parameter (symbol %d) has unresolvable type %s", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type))
			}
			switch kind {
			case width:
				params = append(params, cType(width)+fmt.Sprintf(" pebble_local_%d", param.Symbol))
			case types.Bool:
				params = append(params, fmt.Sprintf("bool pebble_local_%d", param.Symbol))
			default:
				// validateHelperSignature rules any non-width, non-bool
				// parameter out before a reachable helper is ever built, so
				// this branch is defense for hand-built IR only.
				return "", fmt.Errorf("called function symbol %d parameter (symbol %d) has type %s, want %s or bool", helper.decl.Symbol, param.Symbol, describeType(snapshot, param.Type), wantName(width))
			}
			scope[param.Symbol] = localInfo{kind: kind}
			casts = append(casts, fmt.Sprintf("    (void)pebble_local_%d;", param.Symbol))
		}
		statements, err := buildBlock(unit, snapshot, helper.block, scope, 0, width)
		if err != nil {
			return "", err
		}
		paramList := ""
		if len(params) > 0 {
			paramList = ", " + strings.Join(params, ", ")
		}
		castText := ""
		if len(casts) > 0 {
			castText = strings.Join(casts, "\n") + "\n"
		}
		texts = append(texts, fmt.Sprintf(helperFunction, cType(width), helper.decl.Symbol, paramList, castText, statements))
	}
	return strings.Join(texts, "\n"), nil
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
func buildBlock(unit *tir.Unit, snapshot *types.Snapshot, blockID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind) (string, error) {
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
func buildIf(unit *tir.Unit, snapshot *types.Snapshot, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind) (string, error) {
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
func buildWhile(unit *tir.Unit, snapshot *types.Snapshot, whileNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind) (string, error) {
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
func buildLoopBody(unit *tir.Unit, snapshot *types.Snapshot, bodyID tir.NodeID, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind) (string, error) {
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
func buildLoopIf(unit *tir.Unit, snapshot *types.Snapshot, ifNode tir.Node, locals map[symbol.SymbolID]localInfo, depth int, width types.BuiltinKind) (string, error) {
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
func buildLeadingStatement(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
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
			return buildTupleLocalDeclaration(unit, snapshot, statement, initValue, scope, indent, context, width)
		}
		if isArray(snapshot, initValue.Type) {
			return buildArrayLocalDeclaration(unit, snapshot, statement, initValue, scope, indent, context, width)
		}
		if isOptional(snapshot, initValue.Type) {
			// An optional-typed local: its type is the initializer value's
			// Type (the Initialize node carries no Type itself, confirmed
			// against a real fixture — same as tuple/array locals). The
			// supported initializer is SomeOptional (some <expr>); every
			// other optional initializer shape is a clean rejection.
			return buildOptionalLocalDeclaration(unit, snapshot, statement, initValue, scope, indent, context, width)
		}
		if isStruct(snapshot, initValue.Type) {
			// A struct-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as tuple/array/optional locals). The
			// supported initializer is a RecordConstruct (a struct literal);
			// every other struct initializer shape is a clean rejection.
			return buildStructLocalDeclaration(unit, snapshot, statement, initValue, scope, indent, context, width)
		}
		if isStr(snapshot, initValue.Type) {
			// A str-typed local: its type is the initializer value's Type
			// (the Initialize node carries no Type itself, confirmed against
			// a real fixture — same as the compound locals above). The
			// supported initializer is a StringLiteral (a string literal);
			// every other str initializer shape is a clean rejection.
			return buildStrLocalDeclaration(unit, statement, initValue, scope, indent, context)
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
			scope[statement.Symbol] = localInfo{kind: width}
			// A local that a later statement never reads would otherwise
			// trigger -Wunused-variable under the mandated -Wall -Wextra
			// -Werror; a redundant (void) cast is a no-op when the local IS
			// read later, so it is emitted unconditionally rather than
			// tracking whether a use actually follows.
			return fmt.Sprintf("%s%s pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, cType(width), statement.Symbol, initExpr, indent, statement.Symbol), nil
		case types.Bool:
			// A bool local: emitted as a C bool. The bool value grammar is
			// genuinely different from the integer one (no checked
			// arithmetic), so it is built by buildBoolExpr, not buildExpr.
			initExpr, err := buildBoolExpr(unit, snapshot, statement.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			scope[statement.Symbol] = localInfo{kind: types.Bool}
			// Like integer locals (see the width case), a bool local is
			// emitted as a plain (non-const) bool: the Initialize node does
			// not carry let-vs-var, and the checker guarantees any Store
			// this backend sees targets a writable `var`, so const would
			// only be defense-in-depth at the cost of tracking which locals
			// are ever reassigned. The (void) cast guards against
			// -Wunused-variable exactly as the integer case above does.
			return fmt.Sprintf("%sbool pebble_local_%d = %s;\n%s(void)pebble_local_%d;", indent, statement.Symbol, initExpr, indent, statement.Symbol), nil
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
		targetInfo, declared := scope[place.Symbol]
		if !declared {
			return "", fmt.Errorf("%s reassigns symbol %d, which is not a local in scope", context, place.Symbol)
		}
		// The new value is validated and emitted against the local's own
		// declared type: the entry's width for an integer local (buildExpr),
		// the bool grammar for a bool local (buildBoolExpr). A value of the
		// wrong type — a bool assigned to an integer local, or an integer
		// assigned to a bool local — is rejected by the appropriate builder.
		switch targetInfo.kind {
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
			if targetInfo.isStr {
				// A Store whose place names a str-typed local is a whole-str
				// reassignment, which is out of scope this slice (a str local
				// is only ever initialized from a string literal and then
				// compared, never reassigned).
				return "", fmt.Errorf("%s reassigns symbol %d, a str-typed local; reassigning a str is not supported yet", context, place.Symbol)
			}
			if targetInfo.tuple != 0 {
				// A Store whose place names a tuple-typed local is a
				// whole-tuple reassignment, which is out of scope this slice
				// (only element reads of a tuple local are supported, never
				// assignment into or reassignment of one).
				return "", fmt.Errorf("%s reassigns symbol %d, a tuple-typed local of type %s; reassigning a whole tuple is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.tuple))
			}
			if targetInfo.array != 0 {
				return "", fmt.Errorf("%s reassigns symbol %d, an array-typed local of type %s; reassigning a whole array is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.array))
			}
			if targetInfo.optional != 0 {
				return "", fmt.Errorf("%s reassigns symbol %d, an optional-typed local of type %s; reassigning an optional is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.optional))
			}
			if targetInfo.structType != 0 {
				return "", fmt.Errorf("%s reassigns symbol %d, a struct-typed local of type %s; reassigning a whole struct is not supported yet", context, place.Symbol, describeType(snapshot, targetInfo.structType))
			}
			return "", fmt.Errorf("%s reassigns symbol %d, which is a local of type %s, want %s or bool", context, place.Symbol, describeType(snapshot, place.Type), wantName(width))
		}
	default:
		return "", fmt.Errorf("%s statement is a %s, want a local declaration (Initialize) or a reassignment (Store)", context, statement.Kind)
	}
}

// localInfo records what a declared local holds: an ordinary scalar — the
// entry's resolved integer width or bool, in kind — a str value, in isStr, a
// tuple, in tuple (its
// tuple types.TypeID, stable within one Emit call), an array, in array, an
// optional, in optional, or a struct, in structType. The fields are
// mutually exclusive: kind is zero
// for a compound local (a tuple/array/optional/struct is not a
// types.BuiltinKind), isStr is true only for a str local (a str is a
// types.BuiltinKind but has no width or bool grammar this backend builds —
// it is initialized from a string literal and only ever compared, never
// arithmetically combined), and tuple/array/optional/structType are zero for a
// scalar local. A struct value
// rather than a parallel map keeps the scope a single map threaded through
// every builder unchanged in shape — the existing
// `map[symbol.SymbolID]types.BuiltinKind` value type was widened to this struct
// so no call site needed a second argument, the option that changes the fewest
// existing call sites correctly.
type localInfo struct {
	kind       types.BuiltinKind
	isStr      bool
	tuple      types.TypeID
	array      types.TypeID
	optional   types.TypeID
	structType types.TypeID
}

// cloneLocals returns a fresh copy of the given set of in-scope locals. Every
// recursive scope entry in buildBlock copies before extending, so a block's
// own declarations never leak into the map the caller or a sibling scope
// sees — a local declared inside one if arm is invisible to the sibling arm
// and to anything outside the arm.
func cloneLocals(locals map[symbol.SymbolID]localInfo) map[symbol.SymbolID]localInfo {
	cloned := make(map[symbol.SymbolID]localInfo, len(locals))
	for id, kind := range locals {
		cloned[id] = kind
	}
	return cloned
}

// buildTupleLocalDeclaration builds one tuple-typed local's declaration: a
// `pebble_tuple_<typeID>_t pebble_local_<symbol> = { <element>, ... };` whose
// element expressions are the TupleValue initializer's children in order, each
// built by the grammar its own element type selects — buildExpr for an element
// of the entry's width, buildBoolExpr for a bool element. Every element type
// must be exactly the entry's width or bool; anything else (a str element, a
// nested tuple element) is a clean rejection naming the element position, since
// this backend emits exactly those two C field types. The local's scope entry
// records its tuple type (a localInfo with tuple set), so a later element read
// resolves the tuple type being indexed. The initializer must be a TupleValue
// (a tuple literal): initializing a tuple local from any other value — a
// whole-tuple copy of another local, a call, anything else — is a clean
// rejection, keeping this slice's supported initializer exactly the tuple
// literal. Like every scalar local, the declaration is followed by a (void)
// cast against -Wunused-variable.
func buildTupleLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.TupleValue {
		return "", fmt.Errorf("%s declares a tuple-typed local of type %s initialized from a %s, want a TupleValue (a tuple literal); initializing a tuple local from another value is not supported yet", context, tupleTypeName(initValue.Type), initValue.Kind)
	}
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares a tuple-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("%s declares a tuple-typed local of type %s, which has no element list", context, tupleTypeName(initValue.Type))
	}
	if len(initValue.Children) != len(elements) {
		return "", fmt.Errorf("%s declares a tuple-typed local of type %s with %d element expression(s), want %d (one per declared element)", context, tupleTypeName(initValue.Type), len(initValue.Children), len(elements))
	}
	exprs := make([]string, len(elements))
	for i, elementType := range elements {
		switch {
		case isWidth(snapshot, width, elementType):
			elementExpr, err := buildExpr(unit, snapshot, initValue.Children[i], scope, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		case isBool(snapshot, elementType):
			elementExpr, err := buildBoolExpr(unit, snapshot, initValue.Children[i], scope, width)
			if err != nil {
				return "", err
			}
			exprs[i] = elementExpr
		default:
			return "", fmt.Errorf("%s declares a tuple-typed local of type %s whose element %d is %s, want %s or bool", context, tupleTypeName(initValue.Type), i, describeType(snapshot, elementType), wantName(width))
		}
	}
	scope[statement.Symbol] = localInfo{tuple: initValue.Type}
	return fmt.Sprintf("%spebble_tuple_%d_t pebble_local_%d = { %s };\n%s(void)pebble_local_%d;", indent, initValue.Type, statement.Symbol, strings.Join(exprs, ", "), indent, statement.Symbol), nil
}

// buildArrayLocalDeclaration builds a fixed-length C array from an ArrayValue
// literal. Array elements use the same integer/bool builders as scalar locals;
// nested arrays, repeats, and all other element types remain out of scope.
func buildArrayLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.ArrayValue {
		if initValue.Kind == tir.ArrayRepeat {
			return "", fmt.Errorf("%s declares an array-typed local initialized from ArrayRepeat; array repeat initializers are not supported yet", context)
		}
		return "", fmt.Errorf("%s declares an array-typed local of type %s initialized from a %s, want an ArrayValue (an array literal); initializing an array local from another value is not supported yet", context, describeType(snapshot, initValue.Type), initValue.Kind)
	}
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	length, elementType, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("%s declares an array-typed local of type %s, which has no array length and element type", context, describeType(snapshot, initValue.Type))
	}
	if len(initValue.Children) != int(length) {
		return "", fmt.Errorf("%s declares an array-typed local of type %s with %d element expression(s), want %d", context, describeType(snapshot, initValue.Type), len(initValue.Children), length)
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", fmt.Errorf("%s: %v", context, err)
	}
	exprs := make([]string, len(initValue.Children))
	for i, child := range initValue.Children {
		switch {
		case isWidth(snapshot, width, elementType):
			expr, err := buildExpr(unit, snapshot, child, scope, width)
			if err != nil {
				return "", err
			}
			exprs[i] = expr
		case isBool(snapshot, elementType):
			expr, err := buildBoolExpr(unit, snapshot, child, scope, width)
			if err != nil {
				return "", err
			}
			exprs[i] = expr
		default:
			return "", fmt.Errorf("%s declares an array-typed local of type %s whose element type is %s, want %s or bool", context, describeType(snapshot, initValue.Type), describeType(snapshot, elementType), wantName(width))
		}
	}
	scope[statement.Symbol] = localInfo{array: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d[%d] = { %s };\n%s(void)pebble_local_%d;", indent, arrayElementCType(snapshot, width, elementType), statement.Symbol, length, strings.Join(exprs, ", "), indent, statement.Symbol), nil
}

func arrayElementCType(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) string {
	if isBool(snapshot, id) {
		return "bool"
	}
	return cType(width)
}

// buildOptionalLocalDeclaration builds one optional-typed local's declaration:
// a `pebble_optional_<typeID>_t pebble_local_<symbol> = { .has_value = true,
// .value = <expr> };` for a SomeOptional initializer, or
// `{ .has_value = false, .value = 0 }` for a NoneOptional (`none` — the
// payload value is irrelevant when absent, so zero is fine).
// The payload expression is built by the grammar its own type selects —
// buildExpr for an integer payload, buildBoolExpr for a bool payload — exactly
// like the tuple and array element builders. The local's scope entry records
// its optional type (a localInfo with optional set), so a later force-unwrap
// resolves the optional type being unwrapped. Every payload type must be exactly
// the entry's width or bool; anything else is a clean rejection naming the
// payload type, since this backend emits exactly those two C types as the value
// field. Like every scalar local, the declaration is followed by a (void) cast
// against -Wunused-variable.
func buildOptionalLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("%s declares an optional-typed local of type %s, which has no payload type", context, optionalTypeName(initValue.Type))
	}
	switch initValue.Kind {
	case tir.SomeOptional:
		// SomeOptional has exactly one child: the payload expression.
		if len(initValue.Children) != 1 {
			return "", fmt.Errorf("%s declares an optional-typed local from SomeOptional with %d child(ren), want exactly one payload expression", context, len(initValue.Children))
		}
		var valueExpr string
		switch {
		case isWidth(snapshot, width, payloadType):
			expr, err := buildExpr(unit, snapshot, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		case isBool(snapshot, payloadType):
			expr, err := buildBoolExpr(unit, snapshot, initValue.Children[0], scope, width)
			if err != nil {
				return "", err
			}
			valueExpr = expr
		default:
			return "", fmt.Errorf("%s declares an optional-typed local of type %s whose payload is %s, want %s or bool", context, optionalTypeName(initValue.Type), describeType(snapshot, payloadType), wantName(width))
		}
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = true, .value = %s };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, valueExpr, indent, statement.Symbol), nil
	case tir.NoneOptional:
		// NoneOptional has zero children and the payload value is irrelevant
		// when absent; zero is fine.
		scope[statement.Symbol] = localInfo{optional: initValue.Type}
		return fmt.Sprintf("%s%s pebble_local_%d = { .has_value = false, .value = 0 };\n%s(void)pebble_local_%d;", indent, optionalTypeName(initValue.Type), statement.Symbol, indent, statement.Symbol), nil
	default:
		return "", fmt.Errorf("%s declares an optional-typed local of type %s initialized from a %s, want some <expr> or none", context, optionalTypeName(initValue.Type), initValue.Kind)
	}
}

// buildStructLocalDeclaration builds one struct-typed local's declaration: a
// `pebble_struct_<typeID>_t pebble_local_<symbol> = { .pebble_field_<m0> =
// <e0>, .pebble_field_<m1> = <e1> };` whose field initializers are the
// RecordConstruct's Fields, each value built by the grammar its own type
// selects — buildExpr for a field of the entry's width, buildBoolExpr for a
// bool field. The initializer is a C99 designated-initializer brace list
// (`.pebble_field_<member> = <expr>`), not a positional brace list, so the
// construction-site field order a RecordConstruct's Fields carry (which need
// not match the struct's declared order — a site may write Point.{ y = 2, x =
// 1 }) needs no reordering: each designated initializer places its value
// under exactly the C field its member symbol names, regardless of either
// order. Designated initializers are standard C99 and compile clean under
// -Wall -Wextra -Werror (confirmed by a real cc compile through this test
// suite's own harness). Every field type must be exactly the entry's width or
// bool; anything else (a str field, a nested struct field) is a clean
// rejection naming the field position, since this backend emits exactly those
// two C field types. The initializer must be a RecordConstruct (a struct
// literal): initializing a struct local from any other value — a whole-struct
// copy of another local, a call, anything else — is a clean rejection. The
// local's scope entry records its struct type (a localInfo with structType
// set), so a later field read resolves the struct type being projected. Like
// every scalar local, the declaration is followed by a (void) cast against
// -Wunused-variable.
func buildStructLocalDeclaration(unit *tir.Unit, snapshot *types.Snapshot, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string, width types.BuiltinKind) (string, error) {
	if initValue.Kind != tir.RecordConstruct {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s initialized from a %s, want a RecordConstruct (a struct literal); initializing a struct local from another value is not supported yet", context, structTypeName(initValue.Type), initValue.Kind)
	}
	key, ok := snapshot.Key(initValue.Type)
	if !ok {
		return "", fmt.Errorf("%s declares a struct-typed local whose type %d is not in the type snapshot", context, initValue.Type)
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s, which has no nominal declaration", context, structTypeName(initValue.Type))
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s whose declaration symbol %d has no TypeDeclaration in the unit", context, structTypeName(initValue.Type), decl)
	}
	members := typeDecl.Members
	if len(initValue.Fields) != len(members) {
		return "", fmt.Errorf("%s declares a struct-typed local of type %s with %d field initializer(s), want %d (one per declared field)", context, structTypeName(initValue.Type), len(initValue.Fields), len(members))
	}
	inits := make([]string, len(initValue.Fields))
	for i, field := range initValue.Fields {
		declared := false
		for _, member := range members {
			if member == field.Field {
				declared = true
				break
			}
		}
		if !declared {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s with an initializer for symbol %d, which is not one of its declared fields", context, structTypeName(initValue.Type), field.Field)
		}
		valueNode, ok := unit.Node(field.Value)
		if !ok {
			return "", fmt.Errorf("%s declares a struct-typed local of type %s referencing invalid field value node %d", context, structTypeName(initValue.Type), field.Value)
		}
		var expr string
		switch {
		case isWidth(snapshot, width, valueNode.Type):
			built, err := buildExpr(unit, snapshot, field.Value, scope, width)
			if err != nil {
				return "", err
			}
			expr = built
		case isBool(snapshot, valueNode.Type):
			built, err := buildBoolExpr(unit, snapshot, field.Value, scope, width)
			if err != nil {
				return "", err
			}
			expr = built
		default:
			return "", fmt.Errorf("%s declares a struct-typed local of type %s whose field %d is %s, want %s or bool", context, structTypeName(initValue.Type), field.Field, describeType(snapshot, valueNode.Type), wantName(width))
		}
		inits[i] = fmt.Sprintf(".pebble_field_%d = %s", field.Field, expr)
	}
	scope[statement.Symbol] = localInfo{structType: initValue.Type}
	return fmt.Sprintf("%s%s pebble_local_%d = { %s };\n%s(void)pebble_local_%d;", indent, structTypeName(initValue.Type), statement.Symbol, strings.Join(inits, ", "), indent, statement.Symbol), nil
}

// buildStrLocalDeclaration builds one str-typed local's declaration: a
// `PebbleStr pebble_local_<symbol> = { .data = (const uint8_t *)"<escaped>",
// .len = <N> };` whose initializer is a StringLiteral (a string literal) —
// the only supported str initializer this slice builds. PebbleStr is the
// runtime ABI's length-prefixed string type (runtime/include/pebble_rt.h), a
// fixed runtime type rather than a program-specific shape, so the local is
// declared directly as PebbleStr with no typedef. .data points at the
// literal's bytes re-escaped into a safe C string literal by escapeCString
// (the decoded content is not assumed simple — a control character, a quote,
// or a backslash anywhere in it is escaped correctly, with every non-
// printable byte emitted as a fixed-width octal escape so a following digit
// can never be swallowed by C's maximal-munch escape rules); .len is the
// decoded byte length, a compile-time constant known from the literal itself,
// so no runtime strlen is involved. The initializer must be a StringLiteral:
// initializing a str local from any other value — a copy of another str
// local, a call, anything else — is a clean rejection, keeping this slice's
// supported initializer exactly the string literal. The local's scope entry
// records isStr, so a later str ==/!= comparison resolves the operand as a
// str local. Like every scalar local, the declaration is followed by a (void)
// cast against -Wunused-variable.
func buildStrLocalDeclaration(unit *tir.Unit, statement, initValue tir.Node, scope map[symbol.SymbolID]localInfo, indent, context string) (string, error) {
	if initValue.Kind != tir.StringLiteral {
		return "", fmt.Errorf("%s declares a str-typed local initialized from a %s, want a StringLiteral (a string literal); initializing a str local from another value is not supported yet", context, initValue.Kind)
	}
	if initValue.Literal.Kind != tir.LiteralString {
		return "", fmt.Errorf("%s declares a str-typed local from a StringLiteral with literal kind %s, want a decoded string", context, initValue.Literal.Kind)
	}
	text := initValue.Literal.String
	scope[statement.Symbol] = localInfo{isStr: true}
	return fmt.Sprintf("%sPebbleStr pebble_local_%d = { .data = (const uint8_t *)\"%s\", .len = %d };\n%s(void)pebble_local_%d;", indent, statement.Symbol, escapeCString(text), len(text), indent, statement.Symbol), nil
}

// escapeCString re-escapes a string literal's already-decoded byte content
// into the body of a C string literal, producing a C literal that is
// byte-for-byte the original decoded content. The decoded bytes are not
// assumed simple: a literal may contain a control character (\\n, \\t, \\0,
// or any \\xHH byte escape the lexer accepts), a quote, a backslash, or non-
// ASCII UTF-8. A double-quote and a backslash are escaped as the complete C
// escapes \\" and \\\\ (complete escapes cannot absorb a following character).
// Every byte outside printable ASCII (0x20-0x7E) — control characters, NUL,
// and all non-ASCII bytes — is emitted as a fixed-width octal escape \\NNN
// zero-padded to exactly three digits (e.g. \\012 for newline, \\007 for the
// bell byte). Fixed-width octal is the safe choice specifically because C's
// octal escape consumes at most three octal digits, so a \\NNN escape can
// never accidentally absorb a following digit character the way C's \\xHH
// hex escape can (\\x09A is one out-of-range or wrong escape, whereas
// \\011A is the byte 0x09 followed by 'A'). Everything in printable ASCII
// other than the two escaped characters is emitted verbatim. The result is a
// valid C string-literal body (never containing a raw double-quote or
// backslash), so the caller embeds it between two double-quote characters.
func escapeCString(text string) string {
	var b strings.Builder
	for i := 0; i < len(text); i++ {
		c := text[i]
		switch c {
		case '"':
			b.WriteString(`\"`)
		case '\\':
			b.WriteString(`\\`)
		default:
			if c >= 0x20 && c <= 0x7e {
				b.WriteByte(c)
			} else {
				fmt.Fprintf(&b, `\%03o`, c)
			}
		}
	}
	return b.String()
}

// buildCondition builds the C text for one if/while condition. It dispatches
// on the condition node's shape: a direct integer comparison (tir.BinaryValue)
// keeps the existing buildComparison path unchanged, while a bare bool value —
// a bool literal, a reference to an in-scope bool local, a unary ! negation of
// one of those (tir.PrefixValue with the Bang operator), a comparison used as
// a bool operand, or a && / || combination of any of these (a
// tir.ShortCircuitValue) — is routed through buildBoolExpr. Anything else is
// rejected by whichever builder it reaches.
func buildCondition(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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
// str builtin, they are an equality between two str values built by
// buildStrOperand and lowered to the runtime helper
// pebble_rt_str_eq(<left>, <right>) (==) or its negation (!=) — ordering
// comparisons between strs are rejected cleanly, since the checker does not
// reject them from source (confirmed against a real fixture). When both
// operands carry the snapshot's
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
func buildComparison(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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
	if isStr(snapshot, leftOperand.Type) && isStr(snapshot, rightOperand.Type) {
		// An equality between two str values — s == t, s == "hi",
		// "hi" == "ho", and so on. Only ==/!= make sense for str operands;
		// an ordering comparison between strs (s < t) is reachable from real
		// source (confirmed against a real fixture — the checker does not
		// reject it), so it is rejected cleanly here, never guessed. The
		// comparison is built via the runtime helper
		// pebble_rt_str_eq(<left>, <right>), which is byte-for-byte and
		// length-prefixed (no strlen, no NUL-termination dependence): ==
		// emits the call directly and != emits its negation. Each operand is
		// built by buildStrOperand — a reference to an in-scope str local, or
		// a string literal embedded as a PebbleStr compound literal — so a
		// literal operand participates in a comparison without needing a
		// declared local.
		if node.Operator != syntax.Equal && node.Operator != syntax.NotEqual {
			return "", fmt.Errorf("entry function body if condition compares two str operands with operator %s, want == or !=", node.Operator)
		}
		left, err := buildStrOperand(unit, snapshot, node.Children[0], locals)
		if err != nil {
			return "", err
		}
		right, err := buildStrOperand(unit, snapshot, node.Children[1], locals)
		if err != nil {
			return "", err
		}
		if node.Operator == syntax.Equal {
			return "pebble_rt_str_eq(" + left + ", " + right + ")", nil
		}
		return "!pebble_rt_str_eq(" + left + ", " + right + ")", nil
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
func buildComparisonOperand(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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

// buildStrOperand builds one operand of a str ==/!= comparison, which is
// exactly two shapes (both confirmed against a real fixture): a SymbolValue
// naming an in-scope str-typed local (emitted as its pebble_local_<symbolID>
// C name — a PebbleStr lvalue), or a StringLiteral (a str value with no
// local behind it, emitted as a PebbleStr compound literal carrying the
// escaped bytes and their compile-time length, the same construction a
// str-typed local's declaration embeds). Anything else — a reference to a
// non-str local, a call, any other node — is a clean rejection, never a
// guessed lowering.
func buildStrOperand(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo) (string, error) {
	node, ok := unit.Node(id)
	if !ok {
		return "", fmt.Errorf("entry function body expression references invalid node %d", id)
	}
	switch node.Kind {
	case tir.SymbolValue:
		info, declared := locals[node.Symbol]
		if !declared || !info.isStr {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a str-typed local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.StringLiteral:
		if node.Literal.Kind != tir.LiteralString {
			return "", fmt.Errorf("entry function body expression contains a StringLiteral with literal kind %s, want a decoded string", node.Literal.Kind)
		}
		text := node.Literal.String
		return fmt.Sprintf("(PebbleStr){ .data = (const uint8_t *)\"%s\", .len = %d }", escapeCString(text), len(text)), nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want a str-typed local reference or a string literal", node.Kind)
	}
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
//   - DirectCall — a call to another Pebble-convention function whose result
//     is the entry's width (validated by the reachability walk in
//     discoverReachableHelpers). Each call-site argument is built by the
//     grammar its callee parameter resolves to — the entry's width for an
//     integer parameter (this builder), bool for a bool parameter
//     (buildBoolExpr) — so the call emits pebble_fn_<calleeSymbolID>(ctx,
//     <arg0>, <arg1>, ...), with the ctx argument prepended by this backend
//     since the typed IR threads context via ContextAction rather than as an
//     explicit child.
//
// CheckedArithmetic with any other operator (the integral operators that build
// this node but are not yet lowered) is rejected, not guessed. A SymbolValue
// referencing anything not in locals (a global, a symbol from an
// outer/different scope — none of which are reachable from this narrow body
// shape, but checked defensively rather than assumed) is a clean rejection.
// Any other node kind at any position — a non-integer
// operand, CheckedShift, and so on — is a clean rejection naming what was
// found.
// Emitting the checked runtime helpers (rather than raw C operators) is what
// keeps the IR nodes' real overflow and divide-by-zero semantics from silently
// disappearing in the emitted program.
func buildExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local (x!). The child is a
		// SymbolValue naming the optional local, and this node's Type is the
		// unwrapped result type (the entry's width, already gated above). The
		// unwrap is bounds-checked via the runtime helper, passing the
		// optional local's has_value and value fields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_%s(pebble_local_%d.has_value, pebble_local_%d.value)", checkedSuffix(width), child.Symbol, child.Symbol), nil
	case tir.Load:
		// A tuple element or struct field read. Reading one element of a
		// tuple-typed local (`t.1`) is lowered by the checker to a Load of a
		// TuplePlace whose single child is the StoragePlace naming the tuple
		// local, and reading one field of a struct-typed local (`point.x`) to
		// a Load of a FieldPlace whose single child is the StoragePlace naming
		// the struct local (both confirmed against real fixtures); these are
		// the only shapes real source produces for reading an element/field of
		// a compound local (a plain local read is a SymbolValue, not a Load).
		// The Load's Type is the element/field's own type, already gated to
		// the entry's width above, so the element/field must resolve to the
		// entry's width here. The emitted C is
		// pebble_local_<symbol>._<ordinal> for a tuple and
		// pebble_local_<symbol>.pebble_field_<member> for a struct.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(unit, snapshot, place, locals, width, false)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(unit, snapshot, place, locals, width, false)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, or FieldPlace", place.Kind)
		}
		return buildTuplePlaceRead(unit, snapshot, place, locals, width, false)
	case tir.TupleElementValue:
		// The checker produces a TupleElementValue only when a tuple literal is
		// indexed directly — (1, 2).1 — whose child is the TupleValue being
		// indexed and whose element type comes out as the unanchored `int`
		// builtin (confirmed against a real fixture); that shape is out of
		// scope, and its int-typed element fails the width gate above before
		// reaching this case. The only in-scope element read of a tuple local
		// is Load(TuplePlace). This case is therefore defense for hand-built
		// IR matching the local-read shape: a TupleElementValue whose single
		// child is a SymbolValue naming a tuple-typed local is emitted exactly
		// like the Load(TuplePlace) read, and any other base is a clean
		// rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, false)
	case tir.DirectCall:
		// A call to another Pebble-convention function whose result is the
		// entry's own width. The width gate above already
		// checked node.Type (the call's result type, which is the callee's
		// resolved result type) is the entry's width. Context threading is
		// not an explicit IR child — the DirectCall records it as
		// ContextAction (ContextForward for a Pebble-convention call) — so,
		// exactly as the old backend textually injected `context`, this
		// backend prepends ctx as the first C argument itself, the same way
		// pebble_user_main receives it. The callee is a reachable helper
		// emitted as pebble_fn_<calleeSymbolID>; the reachability walk has
		// already validated the callee's signature (including its parameters'
		// types, each the entry's width or bool), so the checks below are
		// defense against hand-built IR, matching the file's style.
		if node.Convention != types.Pebble {
			return "", fmt.Errorf("entry function body expression contains a call using the %s calling convention, want Pebble", callingConventionName(node.Convention))
		}
		if node.ContextAction != tir.ContextForward {
			return "", fmt.Errorf("entry function body expression contains a call that records ContextAction %s, want ForwardCurrentContext (this backend only lowers Pebble-convention calls that thread the context)", node.ContextAction)
		}
		// The callee's own declaration supplies the parameter list that decides
		// each argument's grammar below (the reachability walk in
		// discoverReachableHelpers has already resolved and validated this
		// callee, so the checks here are defense against hand-built IR,
		// matching the file's style).
		calleeDecl, err := findFunctionDeclaration(unit, node.Symbol, "called function")
		if err != nil {
			return "", err
		}
		callArgs, err := buildCallArguments(unit, snapshot, node, calleeDecl, locals, width)
		if err != nil {
			return "", err
		}
		if len(node.TypeArgs) != 0 {
			return "", fmt.Errorf("entry function body expression contains a call to a generic function with %d type argument(s), which this backend does not lower (generics are not supported yet)", len(node.TypeArgs))
		}
		if callArgs == "" {
			return fmt.Sprintf("pebble_fn_%d(ctx)", node.Symbol), nil
		}
		return fmt.Sprintf("pebble_fn_%d(ctx, %s)", node.Symbol, callArgs), nil
	default:
		return "", fmt.Errorf("entry function body expression contains a %s, want an integer literal, a reference to a local declared earlier in the body, checked +, -, *, /, %% arithmetic, or a call to another function", node.Kind)
	}
}

// buildTuplePlaceRead builds the C text for reading one element of a tuple
// local through the Load(TuplePlace) shape the checker actually produces for
// `t.<ordinal>` (confirmed against a real fixture): the TuplePlace carries the
// element Ordinal and its single child is the StoragePlace naming the tuple
// local. wantBool selects which grammar the element must satisfy — bool (the
// buildBoolExpr path) or the entry's width (the buildExpr path) — matching how
// the Load's own Type was already gated by the caller's builder. The emitted C
// is pebble_local_<symbol>._<ordinal>.
func buildTuplePlaceRead(unit *tir.Unit, snapshot *types.Snapshot, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("entry function body expression contains a TuplePlace with %d child(ren), want exactly one (the tuple local's place)", len(place.Children))
	}
	base, ok := unit.Node(place.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a TuplePlace referencing invalid node %d", place.Children[0])
	}
	if base.Kind != tir.StoragePlace {
		return "", fmt.Errorf("entry function body expression contains a TuplePlace whose child is a %s, want a StoragePlace naming a tuple-typed local", base.Kind)
	}
	return buildTupleElement(unit, snapshot, base.Symbol, place.Ordinal, locals, width, wantBool)
}

// buildArrayPlaceRead lowers Load(CheckedIndexPlace) for an array local. The
// index is built as an integer expression and checked with the runtime helper
// selected by the entry width before it is used as the C subscript.
func buildArrayPlaceRead(unit *tir.Unit, snapshot *types.Snapshot, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 2 {
		return "", fmt.Errorf("entry function body expression contains a CheckedIndexPlace with %d child(ren), want exactly two (the array local's place and index)", len(place.Children))
	}
	base, ok := unit.Node(place.Children[0])
	if !ok || base.Kind != tir.StoragePlace {
		kind := "invalid"
		if ok {
			kind = string(base.Kind)
		}
		return "", fmt.Errorf("entry function body expression contains a CheckedIndexPlace whose base is a %s, want a StoragePlace naming an array-typed local", kind)
	}
	info, declared := locals[base.Symbol]
	if !declared || info.array == 0 {
		return "", fmt.Errorf("entry function body expression indexes symbol %d, which is not an array-typed local declared earlier in the entry body", base.Symbol)
	}
	key, ok := snapshot.Key(info.array)
	if !ok {
		return "", fmt.Errorf("entry function body expression indexes an array local whose type %d is not in the type snapshot", info.array)
	}
	length, element, ok := key.Array()
	if !ok {
		return "", fmt.Errorf("entry function body expression indexes local %d whose type is not an array", base.Symbol)
	}
	if _, err := arrayLengthLiteral(length, width); err != nil {
		return "", err
	}
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("entry function body expression indexes array local %d, whose element type is %s, want bool", base.Symbol, describeType(snapshot, element))
		}
	} else if !isWidth(snapshot, width, element) {
		return "", fmt.Errorf("entry function body expression indexes array local %d, whose element type is %s, want %s", base.Symbol, describeType(snapshot, element), wantName(width))
	}
	indexNode, ok := unit.Node(place.Children[1])
	if !ok {
		return "", fmt.Errorf("array index references invalid node %d", place.Children[1])
	}
	var index string
	if indexNode.Kind == tir.IntegerLiteral && indexNode.Type == snapshot.Builtins().Int {
		if !isNonNegativeDecimal(indexNode.Literal.IntegerNum) {
			return "", fmt.Errorf("array index contains an integer literal with malformed text %q", indexNode.Literal.IntegerNum)
		}
		index = indexNode.Literal.IntegerNum
	} else {
		var err error
		index, err = buildExpr(unit, snapshot, place.Children[1], locals, width)
		if err != nil {
			return "", fmt.Errorf("array index: %v", err)
		}
	}
	literal, _ := arrayLengthLiteral(length, width)
	return fmt.Sprintf("pebble_local_%d[pebble_rt_checked_index_%s(%s, %s)]", base.Symbol, checkedSuffix(width), index, literal), nil
}

// buildTupleElement builds the C text for reading one element of a tuple local
// by symbol and ordinal: pebble_local_<symbol>._<ordinal>. The symbol must be a
// local the scope records as tuple-typed (its localInfo.tuple), the ordinal
// must be in range for that tuple type's element list, and the element's own
// type must satisfy the grammar wantBool selects — bool for the buildBoolExpr
// path, the entry's width for the buildExpr path. The tuple type comes from the
// scope record, not from any node field, so a read always resolves against the
// type the local was actually declared with.
func buildTupleElement(unit *tir.Unit, snapshot *types.Snapshot, symbolID symbol.SymbolID, ordinal uint32, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	info, declared := locals[symbolID]
	if !declared || info.tuple == 0 {
		return "", fmt.Errorf("entry function body expression reads an element of symbol %d, which is not a tuple-typed local declared earlier in the entry body", symbolID)
	}
	key, ok := snapshot.Key(info.tuple)
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of a tuple local whose type %d is not in the type snapshot", info.tuple)
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("entry function body expression reads an element of tuple type %s, which has no element list", tupleTypeName(info.tuple))
	}
	if ordinal >= uint32(len(elements)) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d of %s, which has only %d element(s)", ordinal, tupleTypeName(info.tuple), len(elements))
	}
	element := elements[ordinal]
	if wantBool {
		if !isBool(snapshot, element) {
			return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want bool", ordinal, describeType(snapshot, element))
		}
	} else if !isWidth(snapshot, width, element) {
		return "", fmt.Errorf("entry function body expression reads tuple element %d, whose type is %s, want %s", ordinal, describeType(snapshot, element), wantName(width))
	}
	return fmt.Sprintf("pebble_local_%d._%d", symbolID, ordinal), nil
}

// buildStructFieldRead builds the C text for reading one field of a struct
// local through the Load(FieldPlace) shape the checker actually produces for
// `point.x` (confirmed against a real fixture): the FieldPlace carries the
// field's own member symbol in Member and its single child is the StoragePlace
// naming the struct local. wantBool selects which grammar the field must
// satisfy — bool (the buildBoolExpr path) or the entry's width (the buildExpr
// path). The field's own type is resolved from the struct's declared fields by
// matching FieldPlace.Member (see declaredFieldType), not assumed from the
// place's own Type. The emitted C is
// pebble_local_<symbol>.pebble_field_<member>.
func buildStructFieldRead(unit *tir.Unit, snapshot *types.Snapshot, place tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind, wantBool bool) (string, error) {
	if len(place.Children) != 1 {
		return "", fmt.Errorf("entry function body expression contains a FieldPlace with %d child(ren), want exactly one (the struct local's place)", len(place.Children))
	}
	base, ok := unit.Node(place.Children[0])
	if !ok {
		return "", fmt.Errorf("entry function body expression contains a FieldPlace referencing invalid node %d", place.Children[0])
	}
	if base.Kind != tir.StoragePlace {
		// A FieldPlace whose base is not a plain StoragePlace is a nested
		// field access (o.inner.x, whose outer FieldPlace's base is another
		// FieldPlace) or a field read off a non-local value — both confirmed
		// reachable from real source but out of scope this slice, so a clean
		// rejection naming what was found.
		return "", fmt.Errorf("entry function body expression contains a FieldPlace whose base is a %s, want a StoragePlace naming a struct-typed local (nested field access and reading a field off a struct literal are not supported)", base.Kind)
	}
	info, declared := locals[base.Symbol]
	if !declared || info.structType == 0 {
		return "", fmt.Errorf("entry function body expression reads a field of symbol %d, which is not a struct-typed local declared earlier in the entry body", base.Symbol)
	}
	fieldType, ok := declaredFieldType(unit, snapshot, info.structType, place.Member)
	if !ok {
		return "", fmt.Errorf("entry function body expression reads field %d of symbol %d, which is not a declared field of struct type %s", place.Member, base.Symbol, describeType(snapshot, info.structType))
	}
	if wantBool {
		if !isBool(snapshot, fieldType) {
			return "", fmt.Errorf("entry function body expression reads field %d of symbol %d, whose type is %s, want bool", place.Member, base.Symbol, describeType(snapshot, fieldType))
		}
	} else if !isWidth(snapshot, width, fieldType) {
		return "", fmt.Errorf("entry function body expression reads field %d of symbol %d, whose type is %s, want %s", place.Member, base.Symbol, describeType(snapshot, fieldType), wantName(width))
	}
	return fmt.Sprintf("pebble_local_%d.pebble_field_%d", base.Symbol, place.Member), nil
}

// declaredFieldType resolves one field's own type from a struct type's
// declared fields, matching the field's member symbol against the struct's
// TypeDecl.Members list (the declared field order). The FieldDeclaration nodes
// in the unit carry only the field's symbol, never its type, so the type is
// resolved from the unit's own node graph: any FieldPlace node carrying the
// member (its Type is the field's resolved type), or any RecordConstruct of
// the same declaration whose Fields contain the member (the value node's Type
// is the field's resolved type) — both are guaranteed consistent for a real
// fixture, since a struct field has exactly one type. A member that is not in
// the struct's declared member list, or whose type cannot be resolved from the
// unit, reports false.
func declaredFieldType(unit *tir.Unit, snapshot *types.Snapshot, structType types.TypeID, member symbol.SymbolID) (types.TypeID, bool) {
	key, ok := snapshot.Key(structType)
	if !ok {
		return 0, false
	}
	decl, _, ok := key.Nominal()
	if !ok {
		return 0, false
	}
	typeDecl, ok := findTypeDeclaration(unit, decl)
	if !ok {
		return 0, false
	}
	declared := false
	for _, m := range typeDecl.Members {
		if m == member {
			declared = true
			break
		}
	}
	if !declared {
		return 0, false
	}
	for _, node := range unit.Nodes() {
		if node.Kind == tir.FieldPlace && node.Member == member && node.Type != 0 {
			return node.Type, true
		}
		if node.Kind == tir.RecordConstruct && node.Symbol == decl {
			for _, field := range node.Fields {
				if field.Field == member {
					if value, ok := unit.Node(field.Value); ok && value.Type != 0 {
						return value.Type, true
					}
				}
			}
		}
	}
	return 0, false
}

// buildCallArguments builds the comma-separated C argument list for a
// DirectCall's children, one expression per child in order. Each child's
// grammar is decided by the callee's corresponding parameter's resolved type
// — the entry's width parameters take buildExpr, bool parameters take
// buildBoolExpr — so the same two value grammars this backend already builds
// lower the arguments; the checker has already coerced each argument to its
// parameter's type, so a mismatch here is hand-built IR. The argument count
// must equal the callee's declared parameter count. Returns the joined
// argument text, empty when the callee takes no parameters (the caller then
// emits pebble_fn_<id>(ctx) with no argument list).
func buildCallArguments(unit *tir.Unit, snapshot *types.Snapshot, call tir.Node, callee tir.Node, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
	if len(call.Children) != len(callee.Parameters) {
		return "", fmt.Errorf("entry function body expression contains a call to symbol %d passing %d argument(s), want %d (the callee declares %d parameter(s))", call.Symbol, len(call.Children), len(callee.Parameters), len(callee.Parameters))
	}
	args := make([]string, len(call.Children))
	for i, argID := range call.Children {
		param := callee.Parameters[i]
		switch {
		case isWidth(snapshot, width, param.Type):
			arg, err := buildExpr(unit, snapshot, argID, locals, width)
			if err != nil {
				return "", err
			}
			args[i] = arg
		case isBool(snapshot, param.Type):
			arg, err := buildBoolExpr(unit, snapshot, argID, locals, width)
			if err != nil {
				return "", err
			}
			args[i] = arg
		default:
			// validateHelperSignature rules any non-width, non-bool parameter
			// out before a reachable helper is ever built, so this branch is
			// defense for hand-built IR only.
			return "", fmt.Errorf("entry function body expression contains a call to symbol %d whose parameter %d (symbol %d) has type %s, want %s or bool", call.Symbol, i, param.Symbol, describeType(snapshot, param.Type), wantName(width))
		}
	}
	return strings.Join(args, ", "), nil
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
//   - Load of a TuplePlace — a tuple-typed local's bool element read (`t.1`
//     in a bool position), the same Load(TuplePlace) shape buildExpr's Load
//     case handles but with the element's own type gated to bool here, so the
//     read emits pebble_local_<symbol>._<ordinal> via buildTupleElement. (A
//     plain bool local read is a SymbolValue, not a Load.)
//
// A bool-returning call needs no DirectCall case here and has none: a called
// function may only resolve to the entry's integer width or void (see
// validateHelperSignature), and void-result calls are deliberately out of
// scope this slice, so no reachable DirectCall can carry the bool builtin and
// no bool call can reach this builder — confirmed by construction, not
// assumed.
//
// A SymbolValue referencing anything else — an integer local, a global, a
// parameter — and any other node kind at any position is a clean rejection
// naming what was found.
func buildBoolExpr(unit *tir.Unit, snapshot *types.Snapshot, id tir.NodeID, locals map[symbol.SymbolID]localInfo, width types.BuiltinKind) (string, error) {
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
		if locals[node.Symbol].kind != types.Bool {
			return "", fmt.Errorf("entry function body expression references symbol %d, which is not a bool local declared earlier in the entry body", node.Symbol)
		}
		return fmt.Sprintf("pebble_local_%d", node.Symbol), nil
	case tir.CheckedOptionalUnwrap:
		// A force-unwrap of an optional-typed local with a bool payload (x!).
		// The child is a SymbolValue naming the optional local, and this
		// node's Type is bool (already gated above). The unwrap is
		// bounds-checked via the runtime helper, passing the optional local's
		// has_value and value fields.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap with %d child(ren), want exactly one (the optional value being unwrapped)", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing invalid child node %d", node.Children[0])
		}
		if child.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap whose child is a %s, want a SymbolValue naming an optional-typed local", child.Kind)
		}
		info, declared := locals[child.Symbol]
		if !declared {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap referencing symbol %d, which is not a local declared earlier in the entry body", child.Symbol)
		}
		if info.optional == 0 {
			return "", fmt.Errorf("entry function body expression contains a CheckedOptionalUnwrap of symbol %d, which is not an optional-typed local", child.Symbol)
		}
		return fmt.Sprintf("pebble_rt_checked_unwrap_bool(pebble_local_%d.has_value, pebble_local_%d.value)", child.Symbol, child.Symbol), nil
	case tir.Load:
		// A tuple-typed local's bool element read or a struct-typed local's
		// bool field read (see buildExpr's Load case for the shape
		// confirmation; here the Load's Type is the element/field's bool type,
		// already gated above).
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a Load with %d child(ren), want exactly one place", len(node.Children))
		}
		place, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a Load referencing invalid place node %d", node.Children[0])
		}
		if place.Kind != tir.TuplePlace {
			if place.Kind == tir.CheckedIndexPlace {
				return buildArrayPlaceRead(unit, snapshot, place, locals, width, true)
			}
			if place.Kind == tir.FieldPlace {
				return buildStructFieldRead(unit, snapshot, place, locals, width, true)
			}
			return "", fmt.Errorf("entry function body expression contains a Load whose place is a %s, want a TuplePlace, CheckedIndexPlace, or FieldPlace", place.Kind)
		}
		return buildTuplePlaceRead(unit, snapshot, place, locals, width, true)
	case tir.TupleElementValue:
		// Defense for hand-built IR, exactly like buildExpr's TupleElementValue
		// case: the checker never produces this shape for a bool element read of
		// a tuple local (that is a Load of a TuplePlace); a TupleElementValue
		// whose single child is a SymbolValue naming a tuple-typed local is
		// accepted here, anything else is a clean rejection.
		if len(node.Children) != 1 {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue with %d child(ren), want exactly one (the tuple value being indexed)", len(node.Children))
		}
		base, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("entry function body expression contains a TupleElementValue referencing invalid node %d", node.Children[0])
		}
		if base.Kind != tir.SymbolValue {
			return "", fmt.Errorf("entry function body expression reads element %d of a %s, want a SymbolValue naming a tuple-typed local (indexing a tuple literal is not supported)", node.Ordinal, base.Kind)
		}
		return buildTupleElement(unit, snapshot, base.Symbol, node.Ordinal, locals, width, true)
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

// isStr reports whether id is the snapshot's str builtin. A str value is a
// builtin like bool, but unlike bool (or the entry's integer width) it has no
// arithmetic grammar this backend builds — a str local is initialized only
// from a string literal and a str value is only ever an operand of a ==/!=
// comparison — so it is recognized by this distinct predicate rather than by
// a shared scalar-builder switch.
func isStr(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	return id == snapshot.Builtins().Str
}

// isTuple reports whether id resolves to a tuple type in the snapshot. It is
// how the emitter recognizes a tuple-typed local's declaration without
// consulting the builtin table: a tuple is not a types.BuiltinKind, so
// resolvedBuiltin returns no kind for it and the caller must ask whether the
// type is a tuple instead.
func isTuple(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	if !ok {
		return false
	}
	return key.Kind() == types.Tuple
}

// isArray reports whether id resolves to a fixed-length array type.
func isArray(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Array
}

// isOptional reports whether id resolves to an optional type in the snapshot.
func isOptional(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Optional
}

// isStruct reports whether id resolves to a struct type in the snapshot. It is
// how the emitter recognizes a struct-typed local's declaration without
// consulting the builtin table: a struct is a Nominal type, not a
// types.BuiltinKind, so resolvedBuiltin returns no kind for it and the caller
// must ask whether the type is a struct instead. A generic struct's
// monomorphized instance is also Nominal (its Nominal arguments are the
// concrete type arguments), so it is recognized the same way; this backend
// never inspects the argument list, so a generic instance is emitted exactly
// like a non-generic struct of the same shape.
func isStruct(snapshot *types.Snapshot, id types.TypeID) bool {
	if snapshot == nil {
		return false
	}
	key, ok := snapshot.Key(id)
	return ok && key.Kind() == types.Nominal
}

// arrayLengthLiteral validates that the compile-time length can be passed to
// the width-specific checked-index helper without a narrowing conversion.
func arrayLengthLiteral(length uint64, width types.BuiltinKind) (string, error) {
	max := uint64(^uint32(0) >> 1)
	if width == types.I64 {
		max = uint64(^uint64(0) >> 1)
	}
	if length > max {
		return "", fmt.Errorf("array length %d does not fit the %s checked-index helper", length, wantName(width))
	}
	return fmt.Sprintf("%d", length), nil
}

// tupleTypeName is the deterministic C name of one distinct tuple type's
// struct typedef: pebble_tuple_<typeID>_t, derived from the tuple type's own
// stable types.TypeID (stable within one Emit call), mirroring the
// pebble_fn_<symbolID> / pebble_local_<symbolID> naming discipline of reusing
// a stable IR identity rather than a counter.
func tupleTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_tuple_%d_t", id)
}

// optionalTypeName is the deterministic C name of one distinct optional type's
// struct typedef: pebble_optional_<typeID>_t, derived from the optional
// type's own stable types.TypeID, mirroring the tuple naming discipline.
func optionalTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_optional_%d_t", id)
}

// structTypeName is the deterministic C name of one distinct struct type's
// struct typedef: pebble_struct_<typeID>_t, derived from the struct type's own
// stable types.TypeID, mirroring the tuple naming discipline.
func structTypeName(id types.TypeID) string {
	return fmt.Sprintf("pebble_struct_%d_t", id)
}

// tupleElementCType is the C field type a tuple element of the given type is
// declared with in its tuple's struct typedef: int32_t / int64_t for an
// element of the entry's resolved width, bool for a bool element. Any other
// element type is a clean rejection naming what was found, since this backend
// emits exactly those two C types as tuple fields.
func tupleElementCType(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("element type %s is not supported, want %s or bool", name, wantName(width))
		}
	}
	return "", fmt.Errorf("element type %s is not supported, want %s or bool", describeType(snapshot, id), wantName(width))
}

// buildTupleTypedefs builds the C text of one struct typedef per tuple type in
// ids, in order, each joined by a newline. The caller (Emit) supplies ids in
// first-encountered order from the tuple-type collection pass, so every tuple
// type the emitted program references has exactly one typedef here, written
// before any function definition in the final output.
func buildTupleTypedefs(snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildTupleTypedef(snapshot, width, id)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildTupleTypedef builds the C text of one tuple type's struct typedef, with
// positional fields `_0`, `_1`, ... in element order (mirroring the old
// backend's own tuple-field naming convention, without the old 9-field cap):
//
//	typedef struct {
//	    int32_t _0;
//	    bool _1;
//	} pebble_tuple_<typeID>_t;
//
// Each field's C type comes from tupleElementCType, which validates the
// element is the entry's width or bool. A TypeID that is not a tuple type in
// the snapshot is a clean rejection, not a guessed layout.
func buildTupleTypedef(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("tuple type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Tuple {
		return "", fmt.Errorf("type %s is a %v, want a tuple type", tupleTypeName(id), key.Kind())
	}
	elements, ok := key.Elements()
	if !ok {
		return "", fmt.Errorf("tuple type %s has no element list", tupleTypeName(id))
	}
	fields := make([]string, len(elements))
	for i, element := range elements {
		ctype, err := tupleElementCType(snapshot, width, element)
		if err != nil {
			return "", fmt.Errorf("tuple type %s: %v", tupleTypeName(id), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" _%d;", i)
	}
	return fmt.Sprintf("typedef struct {\n%s\n} %s;", strings.Join(fields, "\n"), tupleTypeName(id)), nil
}

// buildOptionalTypedefs builds the C text of one struct typedef per optional
// type in ids, in order, each joined by a newline. The caller (Emit) supplies
// ids in first-encountered order from the optional-type collection pass, so
// every optional type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildOptionalTypedefs(snapshot *types.Snapshot, width types.BuiltinKind, ids []types.TypeID) (string, error) {
	texts := make([]string, 0, len(ids))
	for _, id := range ids {
		text, err := buildOptionalTypedef(snapshot, width, id)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildOptionalTypedef builds the C text of one optional type's struct typedef:
//
//	typedef struct {
//	    bool has_value;
//	    int32_t value;
//	} pebble_optional_<typeID>_t;
//
// The value field's C type is the payload's own type (int32_t/int64_t for the
// entry's width, bool for a bool payload). A TypeID that is not an optional
// type in the snapshot is a clean rejection, not a guessed layout.
func buildOptionalTypedef(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	key, ok := snapshot.Key(id)
	if !ok {
		return "", fmt.Errorf("optional type %d is not in the type snapshot", id)
	}
	if key.Kind() != types.Optional {
		return "", fmt.Errorf("type %s is a %v, want an optional type", optionalTypeName(id), key.Kind())
	}
	payloadType, ok := key.Child()
	if !ok {
		return "", fmt.Errorf("optional type %s has no payload type", optionalTypeName(id))
	}
	valueCType, err := optionalPayloadCType(snapshot, width, payloadType)
	if err != nil {
		return "", fmt.Errorf("optional type %s: %v", optionalTypeName(id), err)
	}
	return fmt.Sprintf("typedef struct {\n    bool has_value;\n    %s value;\n} %s;", valueCType, optionalTypeName(id)), nil
}

// buildStructTypedefs builds the C text of one struct typedef per struct type
// in infos, in order, each joined by a newline. The caller (Emit) supplies
// infos in first-encountered order from the struct-type collection pass, so
// every struct type the emitted program references has exactly one typedef
// here, written before any function definition in the final output.
func buildStructTypedefs(snapshot *types.Snapshot, width types.BuiltinKind, infos []structInfo) (string, error) {
	texts := make([]string, 0, len(infos))
	for _, info := range infos {
		text, err := buildStructTypedef(snapshot, width, info)
		if err != nil {
			return "", err
		}
		texts = append(texts, text)
	}
	return strings.Join(texts, "\n"), nil
}

// buildStructTypedef builds the C text of one struct type's struct typedef,
// with one field per declared struct field, in the struct's *declared* order
// (from the TypeDecl's Members list, resolved by collectStructTypes — never
// the construction-site order a RecordConstruct's Fields carry), each named
// deterministically from the field's own stable symbol.SymbolID:
//
//	typedef struct {
//	    int32_t pebble_field_25;
//	    bool pebble_field_26;
//	} pebble_struct_<typeID>_t;
//
// Naming each C field from the field's symbol ID (mirroring the
// pebble_local_<symbolID> / pebble_fn_<symbolID> discipline) makes a C-field
// name collision impossible even if a source field name were a C keyword or
// duplicated another identifier. Each field's C type comes from
// structFieldCType, which validates the field is the entry's width or bool.
// A structInfo whose TypeID is not a Nominal type in the snapshot is a clean
// rejection, not a guessed layout (defense for hand-built IR; collectStructTypes
// has already resolved every collected TypeID through resolveStructInfo).
func buildStructTypedef(snapshot *types.Snapshot, width types.BuiltinKind, info structInfo) (string, error) {
	key, ok := snapshot.Key(info.typ)
	if !ok {
		return "", fmt.Errorf("struct type %d is not in the type snapshot", info.typ)
	}
	if key.Kind() != types.Nominal {
		return "", fmt.Errorf("type %s is a %v, want a struct type", structTypeName(info.typ), key.Kind())
	}
	fields := make([]string, len(info.fields))
	for i, field := range info.fields {
		ctype, err := structFieldCType(snapshot, width, field.typ)
		if err != nil {
			return "", fmt.Errorf("struct type %s: %v", structTypeName(info.typ), err)
		}
		fields[i] = "    " + ctype + fmt.Sprintf(" pebble_field_%d;", field.member)
	}
	return fmt.Sprintf("typedef struct {\n%s\n} %s;", strings.Join(fields, "\n"), structTypeName(info.typ)), nil
}

// structFieldCType is the C field type a struct field of the given type is
// declared with in its struct's typedef: int32_t / int64_t for a field of the
// entry's resolved width, bool for a bool field. Any other field type — a str
// field, a nested struct field, a tuple/array/optional field — is a clean
// rejection naming what was found, since this backend emits exactly those two
// C types as struct fields.
func structFieldCType(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("field type %s is not supported, want %s or bool", name, wantName(width))
		}
	}
	return "", fmt.Errorf("field type %s is not supported, want %s or bool", describeType(snapshot, id), wantName(width))
}

// optionalPayloadCType is the C field type an optional payload of the given
// type is declared with in its optional's struct typedef: int32_t / int64_t
// for a payload of the entry's resolved width, bool for a bool payload. Any
// other payload type is a clean rejection naming what was found, since this
// backend emits exactly those two C types as optional value fields.
func optionalPayloadCType(snapshot *types.Snapshot, width types.BuiltinKind, id types.TypeID) (string, error) {
	if isWidth(snapshot, width, id) {
		return cType(width), nil
	}
	if isBool(snapshot, id) {
		return "bool", nil
	}
	if builtin, ok := resolvedBuiltin(snapshot, id); ok {
		if name, ok := builtinName(builtin); ok {
			return "", fmt.Errorf("payload type %s is not supported, want %s or bool", name, wantName(width))
		}
	}
	return "", fmt.Errorf("payload type %s is not supported, want %s or bool", describeType(snapshot, id), wantName(width))
}

// joinTypedefs joins two typedef text blocks into a single block, with a blank
// line between them when both are non-empty. Either may be empty; the result is
// empty when both are empty. Emit chains it twice (tuple joined with optional,
// then the result joined with struct) so the three typedef families form one
// block in a fixed order.
func joinTypedefs(tupleTypedefs, optionalTypedefs string) string {
	if tupleTypedefs == "" {
		return optionalTypedefs
	}
	if optionalTypedefs == "" {
		return tupleTypedefs
	}
	return tupleTypedefs + "\n" + optionalTypedefs
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

// helperFunction is the C text of one reachable helper function: a static
// function named deterministically pebble_fn_<symbolID> from the callee's
// stable IR identity (mirroring the pebble_local_<symbolID> naming
// discipline — never a counter), taking the Pebble context the same way
// pebble_user_main does plus one parameter declaration per callee parameter,
// each named pebble_local_<paramSymbol>. %s is the C return type for the
// entry's resolved width (cType), %d the callee's symbol ID, the third %s the
// comma-separated parameter declaration list (", <cType> pebble_local_<id>",
// empty for a zero-parameter callee), the fourth %s one
// `    (void)pebble_local_<id>;` per parameter (suppressing the confirmed
// -Wunused-parameter warning for a parameter the body never reads, the same
// discipline the (void)ctx; below applies to the context), and the last %s the
// helper's body statements built by buildBlock at depth 0 (4-space indent,
// exactly like the entry's own body).
const helperFunction = `static %s pebble_fn_%d(PebbleContext *ctx%s) {
    (void)ctx;
%s%s
}`

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
// confirmed to describe one of the supported program shapes. typedefs is the C
// text of one struct typedef per distinct tuple/optional/struct type the
// program references, written before every function definition (helpers and
// pebble_user_main) since
// C requires a type to be defined before any function's body can use it; it is
// empty when the program has no tuples, optionals, or structs. helpers is the
// C text of every reachable helper function (each a static
// pebble_fn_<symbolID> definition), written before pebble_user_main so a
// called function's definition precedes its use; it is empty when the program
// has no helpers, in which case the emitted text is byte-identical to the
// pre-10.17 skeleton. <stdbool.h> is included unconditionally: it provides
// the C bool keyword and the true / false literals the moment any bool local
// or literal is emitted, and adding it for programs with no bool at all is
// harmless.
func emitEntryC(w io.Writer, typedefs, helpers, userMain, mainBody string) error {
	if _, err := fmt.Fprint(w, `#include "pebble_rt.h"
#include <stdbool.h>
`); err != nil {
		return err
	}
	if typedefs != "" {
		if _, err := fmt.Fprint(w, "\n"+typedefs+"\n"); err != nil {
			return err
		}
	}
	if helpers != "" {
		if _, err := fmt.Fprint(w, "\n"+helpers+"\n"); err != nil {
			return err
		}
	}
	_, err := fmt.Fprintf(w, `
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
