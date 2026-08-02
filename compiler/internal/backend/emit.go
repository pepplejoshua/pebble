// Package backend lowers typed IR to C source emitted against the versioned
// runtime ABI (runtime/include/pebble_rt.h). It is deliberately narrow: the
// current slice emits exactly one program shape, an empty-bodied Pebble-
// convention void entry function, and rejects everything else with a
// descriptive error instead of guessing.
package backend

import (
	"fmt"
	"io"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// Emit writes C source for unit's designated entry function to w. The entry
// function (identified by entrySymbol) must be Pebble-convention, take zero
// parameters, return void, and have a completely empty body (no statements —
// only ever an ImplicitReturn, i.e. exactly what `fn main() void {}`
// produces). Any other shape returns a descriptive error and writes nothing
// to w; this package does not yet lower expressions or statements.
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
	if err := validateEntrySignature(decl, snapshot); err != nil {
		return err
	}
	block, err := findEntryBody(unit, decl)
	if err != nil {
		return err
	}
	if err := validateEmptyBody(unit, block); err != nil {
		return err
	}
	return emitEntryC(w)
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
// count, and result type against the single supported shape.
func validateEntrySignature(decl tir.Node, snapshot *types.Snapshot) error {
	if decl.Convention != types.Pebble {
		return fmt.Errorf("entry function uses %s calling convention, want Pebble", callingConventionName(decl.Convention))
	}
	if len(decl.Parameters) != 0 {
		return fmt.Errorf("entry function has %d parameter(s), want 0 (main([]str) and main(i32, []str) are not supported yet)", len(decl.Parameters))
	}
	key, ok := snapshot.Key(decl.ResultType)
	if !ok {
		return fmt.Errorf("entry function result type %d is not in the type snapshot", decl.ResultType)
	}
	if builtin, ok := key.Builtin(); !ok || builtin != types.Void {
		return fmt.Errorf("entry function result type is %s, want void", describeType(snapshot, decl.ResultType))
	}
	return nil
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

// emitEntryC writes the fixed adapter shape once the typed IR has been
// confirmed to describe it: a Pebble-convention user entry taking the context,
// and a hosted C main that builds a default context and drives it.
func emitEntryC(w io.Writer) error {
	_, err := io.WriteString(w, `#include "pebble_rt.h"

static void pebble_user_main(PebbleContext *ctx) {
    (void)ctx;
}

int main(int argc, const char **argv) {
    (void)argc;
    (void)argv;
    PebbleContext ctx = pebble_rt_default_context();
    pebble_user_main(&ctx);
    return 0;
}
`)
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
