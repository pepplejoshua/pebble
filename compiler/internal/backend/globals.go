package backend

import (
	"fmt"
	"sort"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// globalInfo is the backend's resolved classification of one mutable
// module-level (`var`) global: the global's own types.TypeID (typ) and the
// equivalent localInfo a local of the same type would carry (info). The
// checker only ever records an initializer (and therefore reaches this
// backend) for a `var` global — an immutable `let` global's value is inlined
// at each reference site and never resolves to storage — so a symbol present
// in emitGlobals is always a mutable global that real file-scope storage
// backs.
type globalInfo struct {
	typ  types.TypeID
	info localInfo
}

// emitGlobals is the set of mutable module-level globals the current Emit
// invocation resolves reads and writes against, scoped to one Emit exactly
// like emitSymbols (set at the top of Emit, cleared by the same deferred
// call, guarded by the same reentrancy panic). It holds only globals actually
// referenced by the reachable program — those are the only ones real storage
// is emitted for (a storage declaration for an unreferenced global would trip
// -Wunused-variable under the mandated -Wall -Wextra -Werror build) — and the
// only ones a read/write reference can resolve to. The map is package-level
// shared mutable state by the same deliberate tradeoff as emitSymbols: this
// package assumes Emit is single-threaded and non-reentrant, and every builder
// in the ~24-site read/write path reads it without threading a new parameter.
var emitGlobals map[symbol.SymbolID]globalInfo

// globalCName returns the C file-scope storage name of a mutable global
// (pebble_global_<symbolID>) when the symbol is one of the current Emit's
// referenced globals. It is the single name source for every global read and
// write resolution site, exactly as pebble_local_<symbolID> is for locals.
func globalCName(symbolID symbol.SymbolID) (string, bool) {
	if _, ok := emitGlobals[symbolID]; !ok {
		return "", false
	}
	return fmt.Sprintf("pebble_global_%d", symbolID), true
}

// localOrGlobalName resolves a symbol reference that must be either an
// already-declared local in scope or a mutable module-level global: a local
// resolves to its own pebble_local_<symbolID> C name, a global to its
// pebble_global_<symbolID> C name. ok is false when the symbol is neither —
// the caller then reports its own "not a local" error.
func localOrGlobalName(symbolID symbol.SymbolID, locals map[symbol.SymbolID]localInfo) (string, bool) {
	if _, declared := locals[symbolID]; declared {
		return fmt.Sprintf("pebble_local_%d", symbolID), true
	}
	return globalCName(symbolID)
}

// globalDeclBySymbol returns the unit's GlobalDecl container for one symbol.
func globalDeclBySymbol(unit *tir.Unit, symbolID symbol.SymbolID) (tir.GlobalDecl, bool) {
	for _, g := range unit.GlobalDeclarations() {
		if g.Symbol == symbolID {
			return g, true
		}
	}
	return tir.GlobalDecl{}, false
}

// resolveGlobalInfo classifies one global declaration's Type into the
// localInfo shape a local of the same type would carry, mirroring how
// helperSignature seeds a parameter's locals scope. Every type the backend
// can declare as a local is classified here; buildGlobalStorage rejects the
// aggregate/slice/optional/pointer/function shapes it cannot yet initialize
// with a C static initializer, never guessing.
func resolveGlobalInfo(unit *tir.Unit, snapshot *types.Snapshot, g tir.GlobalDecl) (globalInfo, error) {
	info := globalInfo{typ: g.Type}
	switch {
	case isStr(snapshot, g.Type):
		info.info = localInfo{isStr: true}
		return info, nil
	case isChar(snapshot, g.Type):
		info.info = localInfo{isChar: true}
		return info, nil
	case isBool(snapshot, g.Type):
		info.info = localInfo{kind: types.Bool}
		return info, nil
	case isFloat(snapshot, g.Type):
		info.info = localInfo{kind: resolvedFloatKind(snapshot, g.Type)}
		return info, nil
	case isEnumType(unit, snapshot, g.Type):
		info.info = localInfo{enumType: g.Type}
		return info, nil
	case isTuple(snapshot, g.Type):
		info.info = localInfo{tuple: g.Type}
		return info, nil
	case isArray(snapshot, g.Type):
		info.info = localInfo{array: g.Type, arrayWrapped: true}
		return info, nil
	case isOptional(snapshot, g.Type):
		info.info = localInfo{optional: g.Type}
		return info, nil
	case isStruct(snapshot, g.Type):
		info.info = localInfo{structType: g.Type}
		return info, nil
	case isSlice(snapshot, g.Type):
		info.info = localInfo{sliceType: g.Type}
		return info, nil
	case isPointer(snapshot, g.Type):
		info.info = localInfo{pointerType: g.Type}
		return info, nil
	case isFunctionType(snapshot, g.Type):
		info.info = localInfo{functionType: g.Type}
		return info, nil
	case isUint(snapshot, g.Type):
		info.info = localInfo{kind: types.Uint}
		return info, nil
	case isU64(snapshot, g.Type):
		info.info = localInfo{kind: types.U64}
		return info, nil
	}
	kind, ok := resolvedBuiltin(snapshot, g.Type)
	if !ok {
		return info, fmt.Errorf("global symbol %d has type %s, which the backend cannot store", g.Symbol, describeType(snapshot, g.Type))
	}
	info.info = localInfo{kind: kind}
	return info, nil
}

// collectReferencedGlobals walks the entry body and every reachable helper
// body, returning the set of mutable globals (globals, the unit's var globals
// — the caller seeds it with exactly the GlobalDeclarations whose Initializer
// is non-zero) that are actually referenced there — as a SymbolValue read or
// as a Store/CompoundStore storage place. The walk is restricted to the
// reachable program so storage is emitted only for globals the emitted C
// actually uses: a global referenced only by an unreachable helper — or never
// referenced at all — would otherwise get an unused static declaration that
// fails -Wunused-variable.
func collectReferencedGlobals(unit *tir.Unit, entryBlock tir.NodeID, helpers []helperInfo, globals map[symbol.SymbolID]struct{}) map[symbol.SymbolID]struct{} {
	referenced := make(map[symbol.SymbolID]struct{})
	seen := make(map[tir.NodeID]bool)
	mark := func(symbolID symbol.SymbolID) {
		if _, isGlobal := globals[symbolID]; isGlobal {
			referenced[symbolID] = struct{}{}
		}
	}
	var walk func(id tir.NodeID)
	walk = func(id tir.NodeID) {
		if id == 0 || seen[id] {
			return
		}
		seen[id] = true
		node, ok := unit.Node(id)
		if !ok {
			return
		}
		switch node.Kind {
		case tir.SymbolValue:
			mark(node.Symbol)
		case tir.Store, tir.CompoundStore:
			if len(node.Children) >= 1 {
				if place, ok := unit.Node(node.Children[0]); ok && place.Kind == tir.StoragePlace {
					mark(place.Symbol)
				}
			}
		}
		for _, child := range node.Children {
			walk(child)
		}
	}
	walk(entryBlock)
	for _, h := range helpers {
		walk(h.block)
	}
	return referenced
}

// buildGlobalStorage returns the C file-scope declaration text for every
// mutable global the current Emit references, one `static <ctype>
// pebble_global_<symbolID> = <initializer>;` per global, in symbol-ID order
// (deterministic regardless of map iteration order). The initializer is the
// global's compile-time-constant initializer recorded by the checker; see
// buildGlobalInitializerText.
func buildGlobalStorage(unit *tir.Unit, snapshot *types.Snapshot) (string, error) {
	ids := make([]symbol.SymbolID, 0, len(emitGlobals))
	for id := range emitGlobals {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	lines := make([]string, 0, len(ids))
	for _, id := range ids {
		ctype, err := globalStorageCType(unit, snapshot, emitGlobals[id].info)
		if err != nil {
			return "", fmt.Errorf("global symbol %d: %v", id, err)
		}
		initializer, err := buildGlobalInitializerText(unit, snapshot, emitGlobals[id].typ, id)
		if err != nil {
			return "", err
		}
		lines = append(lines, fmt.Sprintf("static %s pebble_global_%d = %s;", ctype, id, initializer))
	}
	return strings.Join(lines, "\n"), nil
}

// globalStorageCType returns the C file-scope type a mutable global is
// declared with — the same C type a local of the same Pebble type is declared
// with (cType for a fixed-width integer, bool for bool, int32_t for char,
// floatCType for a float, PebbleStr for str, and the type's own typedef name
// for an enum). The aggregate shapes are clean rejections, not guesses: the
// backend cannot initialize a tuple/struct/array/slice/optional/pointer/
// function global with a C static initializer yet.
func globalStorageCType(unit *tir.Unit, snapshot *types.Snapshot, info localInfo) (string, error) {
	switch {
	case info.isStr:
		return "PebbleStr", nil
	case info.isChar:
		return "int32_t", nil
	case info.enumType != 0:
		if isTaggedUnionType(unit, snapshot, info.enumType) {
			return unionTypeName(info.enumType), nil
		}
		return enumTypeName(info.enumType), nil
	}
	switch info.kind {
	case types.F32, types.F64:
		return floatCType(info.kind), nil
	case types.Bool:
		return "bool", nil
	case types.Uint, types.U8, types.U16, types.U32, types.U64, types.Int, types.I8, types.I16, types.I32, types.I64:
		if cType(info.kind) != "" {
			return cType(info.kind), nil
		}
	}
	if info.tuple != 0 {
		return "", fmt.Errorf("tuple-typed globals are not supported yet")
	}
	if info.array != 0 {
		return "", fmt.Errorf("array-typed globals are not supported yet")
	}
	if info.optional != 0 {
		return "", fmt.Errorf("optional-typed globals are not supported yet")
	}
	if info.structType != 0 {
		return "", fmt.Errorf("struct-typed globals are not supported yet")
	}
	if info.sliceType != 0 {
		return "", fmt.Errorf("slice-typed globals are not supported yet")
	}
	if info.pointerType != 0 {
		return "", fmt.Errorf("pointer-typed globals are not supported yet")
	}
	if info.functionType != 0 {
		return "", fmt.Errorf("function-typed globals are not supported yet")
	}
	return "", fmt.Errorf("has a type the backend cannot store")
}

// buildGlobalInitializerText emits the C static initializer for one mutable
// global's recorded initializer node. The checker guarantees every global
// initializer is a compile-time constant (C0616), but only the constant LEAF
// node shapes are C static-initializer expressions in their own right: an
// IntegerLiteral, BoolLiteral, CharLiteral, FloatLiteral, or StringLiteral
// (a PebbleStr brace initializer), a NilPointer, a payload-less
// EnumVariantValue/VariantConstruct, and a transparent SourceAlias unwrap.
// Any other constant shape — a CheckedArithmetic over literals (`var x int =
// 1 + 2;`), an IntegerCast, or a variant construction with a payload — would
// lower to a runtime call or compound literal that is not a valid C static
// initializer, so it is a clean rejection naming what was found, never a
// guessed emission. (Backend-side constant folding, or checker-side
// serialization of the folded constant, is the natural follow-up that would
// admit those shapes.)
func buildGlobalInitializerText(unit *tir.Unit, snapshot *types.Snapshot, typ types.TypeID, symbolID symbol.SymbolID) (string, error) {
	g, ok := globalDeclBySymbol(unit, symbolID)
	if !ok || g.Initializer == 0 {
		return "", fmt.Errorf("global symbol %d has no recorded initializer", symbolID)
	}
	node, ok := unit.Node(g.Initializer)
	if !ok {
		return "", fmt.Errorf("global symbol %d has an invalid initializer node %d", symbolID, g.Initializer)
	}
	text, err := buildConstantInitializer(unit, snapshot, node, emitGlobals[symbolID].info)
	if err != nil {
		return "", fmt.Errorf("global symbol %d initializer: %v", symbolID, err)
	}
	return text, nil
}

// buildConstantInitializer is the recursive core of buildGlobalInitializerText
// (see its doc comment for the supported shape set). info is the global's
// resolved localInfo, used to pick the right fixed-width integer literal text.
func buildConstantInitializer(unit *tir.Unit, snapshot *types.Snapshot, node tir.Node, info localInfo) (string, error) {
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return "", fmt.Errorf("contains an integer literal with malformed text %q", text)
		}
		return integerLiteralText(text, info.kind), nil
	case tir.BoolLiteral:
		if node.Literal.Bool {
			return "true", nil
		}
		return "false", nil
	case tir.CharLiteral:
		return buildCharLiteralValue(node)
	case tir.FloatLiteral:
		text := node.Literal.Float
		if !isValidFloatLiteralText(text) {
			return "", fmt.Errorf("contains a float literal with malformed text %q", text)
		}
		return text, nil
	case tir.StringLiteral:
		return buildStrLiteralValue(node)
	case tir.NilPointer:
		return "NULL", nil
	case tir.EnumVariantValue:
		if len(node.Children) >= 1 {
			return "", fmt.Errorf("constructs enum variant symbol %d with a payload; a tagged-union construction is not a C static initializer", node.Member)
		}
		return enumVariantName(node.Member), nil
	case tir.VariantConstruct:
		if len(node.Children) >= 1 {
			return "", fmt.Errorf("constructs enum variant symbol %d with a payload; a tagged-union construction is not a C static initializer", node.Member)
		}
		return enumVariantName(node.Member), nil
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return "", fmt.Errorf("contains a SourceAlias with %d child(ren), want exactly one", len(node.Children))
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return "", fmt.Errorf("contains a SourceAlias referencing invalid child node %d", node.Children[0])
		}
		return buildConstantInitializer(unit, snapshot, child, info)
	default:
		return "", fmt.Errorf("contains a %s, which is not a literal constant; only literal constant initializers are supported for mutable globals yet (an arithmetic or cast constant expression is not a C static-initializable expression)", node.Kind)
	}
}
