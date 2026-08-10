package backend

import (
	"fmt"
	"math/big"
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

// localOrGlobalName resolves a symbol reference that must be an already-
// declared local in scope, a mutable module-level global, or an extern
// variable: a local resolves to its own pebble_local_<symbolID> C name, a
// global to its pebble_global_<symbolID> C name, and an extern variable to its
// real C name (errno). ok is false when the symbol is none of these — the
// caller then reports its own "not a local" error.
func localOrGlobalName(symbolID symbol.SymbolID, locals map[symbol.SymbolID]localInfo) (string, bool) {
	if _, declared := locals[symbolID]; declared {
		return fmt.Sprintf("pebble_local_%d", symbolID), true
	}
	if name, ok := globalCName(symbolID); ok {
		return name, true
	}
	return externDataCName(symbolID)
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
	info, err := resolveTypedInfo(unit, snapshot, g.Type)
	if err != nil {
		return globalInfo{typ: g.Type}, fmt.Errorf("global symbol %d %v", g.Symbol, err)
	}
	return globalInfo{typ: g.Type, info: info}, nil
}

// resolveTypedInfo classifies one types.TypeID into the localInfo shape a local
// of the same type would carry, mirroring how helperSignature seeds a
// parameter's locals scope. Every type the backend can declare as a local is
// classified here; the caller wraps the single unclassifiable case (a type the
// backend cannot store) with its own declaration-context error text.
func resolveTypedInfo(unit *tir.Unit, snapshot *types.Snapshot, typ types.TypeID) (localInfo, error) {
	switch {
	case isStr(snapshot, typ):
		return localInfo{isStr: true}, nil
	case isChar(snapshot, typ):
		return localInfo{isChar: true}, nil
	case isBool(snapshot, typ):
		return localInfo{kind: types.Bool}, nil
	case isFloat(snapshot, typ):
		return localInfo{kind: resolvedFloatKind(snapshot, typ)}, nil
	case isEnumType(unit, snapshot, typ):
		return localInfo{enumType: typ}, nil
	case isTuple(snapshot, typ):
		return localInfo{tuple: typ}, nil
	case isArray(snapshot, typ):
		return localInfo{array: typ, arrayWrapped: true}, nil
	case isOptional(snapshot, typ):
		return localInfo{optional: typ}, nil
	case isStruct(snapshot, typ):
		return localInfo{structType: typ}, nil
	case isSlice(snapshot, typ):
		return localInfo{sliceType: typ}, nil
	case isPointer(snapshot, typ):
		return localInfo{pointerType: typ}, nil
	case isFunctionType(snapshot, typ):
		return localInfo{functionType: typ}, nil
	case isUint(snapshot, typ):
		return localInfo{kind: types.Uint}, nil
	case isU64(snapshot, typ):
		return localInfo{kind: types.U64}, nil
	}
	kind, ok := resolvedBuiltin(snapshot, typ)
	if !ok {
		return localInfo{}, fmt.Errorf("has type %s, which the backend cannot store", describeType(snapshot, typ))
	}
	return localInfo{kind: kind}, nil
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
		ctype, err := globalStorageCType(unit, snapshot, emitGlobals[id].info, "global")
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

// globalStorageCType returns the C file-scope type a mutable global or an
// extern variable is declared with — the same C type a local of the same
// Pebble type is declared with (cType for a fixed-width integer, bool for bool,
// int32_t for char, floatCType for a float, PebbleStr for str, and the type's
// own typedef name for an enum). what names the declaration being typed in the
// clean-rejection error text ("global" or "extern variable"), so an aggregate
// extern variable reports itself as such rather than as a global. The
// aggregate shapes are clean rejections, not guesses: the backend cannot
// initialize a tuple/struct/array/slice/optional/pointer/function global with
// a C static initializer, and cannot sensibly declare an extern variable of
// one of those shapes (whose real C layout lives in another translation unit)
// either.
func globalStorageCType(unit *tir.Unit, snapshot *types.Snapshot, info localInfo, what string) (string, error) {
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
		return "", fmt.Errorf("tuple-typed %s are not supported yet", what)
	}
	if info.array != 0 {
		return "", fmt.Errorf("array-typed %s are not supported yet", what)
	}
	if info.optional != 0 {
		return "", fmt.Errorf("optional-typed %s are not supported yet", what)
	}
	if info.structType != 0 {
		return "", fmt.Errorf("struct-typed %s are not supported yet", what)
	}
	if info.sliceType != 0 {
		return "", fmt.Errorf("slice-typed %s are not supported yet", what)
	}
	if info.pointerType != 0 {
		return "", fmt.Errorf("pointer-typed %s are not supported yet", what)
	}
	if info.functionType != 0 {
		return "", fmt.Errorf("function-typed %s are not supported yet", what)
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
// A CheckedArithmetic tree whose operands are, recursively, all integer
// literals is folded here in Go (see foldConstantIntegerTree): the result is
// verified to fit the global's declared type range and emitted as a plain
// literal C constant, reproducing the overflow check the runtime arithmetic
// helper would have performed, statically. Any other constant shape — an
// IntegerCast, a variant construction with a payload, a CheckedArithmetic
// tree containing a non-literal operand, or any other expression that would
// lower to a runtime call or compound literal that is not a valid C static
// initializer — is a clean rejection naming what was found, never a guessed
// emission.
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
// resolved localInfo, used to pick the right fixed-width integer literal text
// and to verify a folded arithmetic result fits the global's declared range.
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
	case tir.CheckedArithmetic:
		value, foldable, err := foldConstantIntegerTree(unit, node)
		if err != nil {
			return "", err
		}
		if !foldable {
			return "", notLiteralConstantError(node.Kind)
		}
		min, max, ok := integerKindRange(info.kind)
		if !ok {
			return "", notLiteralConstantError(node.Kind)
		}
		if value.Cmp(min) < 0 || value.Cmp(max) > 0 {
			name, _ := builtinName(info.kind)
			return "", fmt.Errorf("constant initializer folds to %s, which is outside the global's %s type range (%s..%s)", value, name, min, max)
		}
		return integerLiteralText(value.String(), info.kind), nil
	default:
		return "", notLiteralConstantError(node.Kind)
	}
}

// notLiteralConstantError is the exact shared rejection for a constant global
// initializer shape this backend cannot turn into a C static initializer. The
// text is the agreed "not a literal constant" message; callers that fold a
// shape in (see the CheckedArithmetic case) fall back to it unchanged when
// the tree is not foldable, so the rejection stays byte-identical for any
// expression the folder declines.
func notLiteralConstantError(kind tir.NodeKind) error {
	return fmt.Errorf("contains a %s, which is not a literal constant; only literal constant initializers are supported for mutable globals yet (an arithmetic or cast constant expression is not a C static-initializable expression)", kind)
}

// foldConstantIntegerTree recursively folds a constant-initializer value node
// into its exact integer value, or reports that it is not foldable. The only
// accepted shapes are an IntegerLiteral leaf (its own non-negative decimal
// value) and a CheckedArithmetic whose operator is +, -, *, /, or % and whose
// two operands both fold recursively; a transparent SourceAlias unwraps to
// its single child. Every other node kind — a SymbolValue, a DirectCall, a
// BoolLiteral or FloatLiteral, a CheckedNegate, anything that is not an
// integer literal or integer arithmetic operator — is not foldable, and the
// caller falls back to the exact pre-existing rejection. Arithmetic runs in
// arbitrary precision (math/big), so an intermediate product or sum cannot
// overflow or panic while folding; only the final result is narrowed, and
// only after the caller has verified it fits the target type's range. Folding
// uses truncated division and remainder (big.Int's Quo/Rem), the same
// semantics the runtime checked-div/mod helpers execute.
func foldConstantIntegerTree(unit *tir.Unit, node tir.Node) (*big.Int, bool, error) {
	switch node.Kind {
	case tir.IntegerLiteral:
		text := node.Literal.IntegerNum
		if !isNonNegativeDecimal(text) {
			return nil, false, nil
		}
		value, ok := new(big.Int).SetString(text, 10)
		if !ok {
			return nil, false, nil
		}
		return value, true, nil
	case tir.SourceAlias:
		if len(node.Children) != 1 {
			return nil, false, nil
		}
		child, ok := unit.Node(node.Children[0])
		if !ok {
			return nil, false, nil
		}
		return foldConstantIntegerTree(unit, child)
	case tir.CheckedArithmetic:
		op, ok := arithmeticOperator(node.Operator)
		if !ok || len(node.Children) != 2 {
			return nil, false, nil
		}
		leftNode, ok := unit.Node(node.Children[0])
		if !ok {
			return nil, false, nil
		}
		rightNode, ok := unit.Node(node.Children[1])
		if !ok {
			return nil, false, nil
		}
		left, foldable, err := foldConstantIntegerTree(unit, leftNode)
		if err != nil || !foldable {
			return nil, foldable, err
		}
		right, foldable, err := foldConstantIntegerTree(unit, rightNode)
		if err != nil || !foldable {
			return nil, foldable, err
		}
		result := new(big.Int)
		switch op {
		case "+":
			result.Add(left, right)
		case "-":
			result.Sub(left, right)
		case "*":
			result.Mul(left, right)
		case "/":
			if right.Sign() == 0 {
				return nil, false, fmt.Errorf("constant initializer divides by zero")
			}
			result.Quo(left, right)
		case "%":
			if right.Sign() == 0 {
				return nil, false, fmt.Errorf("constant initializer takes modulo by zero")
			}
			result.Rem(left, right)
		}
		return result, true, nil
	default:
		return nil, false, nil
	}
}

// integerKindRange returns the inclusive [min, max] value range a value of the
// given integer builtin kind can hold, using the kind's true bit width and
// signedness (int is 32-bit signed, uint is 64-bit unsigned, exactly as their
// C representations int32_t and uint64_t declare). ok is false for any
// non-integer kind.
func integerKindRange(kind types.BuiltinKind) (min, max *big.Int, ok bool) {
	var bits uint
	signed := false
	switch kind {
	case types.Int:
		bits, signed = 32, true
	case types.I8:
		bits, signed = 8, true
	case types.I16:
		bits, signed = 16, true
	case types.I32:
		bits, signed = 32, true
	case types.I64:
		bits, signed = 64, true
	case types.Uint:
		bits = 64
	case types.U8:
		bits = 8
	case types.U16:
		bits = 16
	case types.U32:
		bits = 32
	case types.U64:
		bits = 64
	default:
		return nil, nil, false
	}
	if signed {
		// min = -2^(bits-1), max = 2^(bits-1) - 1.
		max = new(big.Int).Sub(new(big.Int).Lsh(big.NewInt(1), bits-1), big.NewInt(1))
		min = new(big.Int).Neg(new(big.Int).Lsh(big.NewInt(1), bits-1))
		return min, max, true
	}
	// min = 0, max = 2^bits - 1.
	max = new(big.Int).Sub(new(big.Int).Lsh(big.NewInt(1), bits), big.NewInt(1))
	return big.NewInt(0), max, true
}
