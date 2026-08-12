package backend

import (
	"fmt"
	"sort"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
	"github.com/pepplejoshua/pebble/compiler/internal/tir"
	"github.com/pepplejoshua/pebble/compiler/internal/types"
)

// externDataInfo is the backend's resolved classification of one extern
// variable (`extern { var errno int; }` or its `let` form): the binding's own
// types.TypeID (typ), the equivalent localInfo a local of the same type would
// carry (info), and the variable's REAL C name (name, errno — resolved from
// the symbol table exactly as externCName resolves an extern function's name).
// An extern variable's storage lives outside this translation unit (inside
// libc, or in a C file the program is linked against), so unlike a mutable
// module-level global it is never DEFINED here — the backend emits only a
// forward `extern <ctype> <name>;` declaration and references the variable by
// its real C name everywhere, exactly as extern functions are called by their
// real C names.
type externDataInfo struct {
	typ  types.TypeID
	info localInfo
	name string
}

// externDataCName returns the real C name (errno) of an extern variable that
// is one of the current Emit's referenced extern data. It is the single name
// source for every extern-variable read and write resolution site, exactly as
// globalCName is for globals — a reference resolves to the variable's real C
// name, never a synthesized pebble_global_<symbolID>.
func externDataCName(st *emitState, symbolID symbol.SymbolID) (string, bool) {
	info, ok := st.externData[symbolID]
	if !ok {
		return "", false
	}
	return info.name, true
}

// externDataName returns the real C name an extern variable must be declared
// and referenced by, mapping the extern binding's stable symbol.SymbolID back
// to the authored identifier via the symbol table threaded into Emit
// (emitState.symbols) — exactly the lookup externCName performs for extern
// functions, with extern-variable-appropriate error text. A nil or missing
// symbol table is a clean error, never a guessed name: an extern variable
// referenced under a made-up name would emit an undeclared identifier that
// fails the mandated -Werror build.
func externDataName(st *emitState, symbolID symbol.SymbolID) (string, error) {
	if st.symbols == nil || st.symbols.Symbols == nil {
		return "", fmt.Errorf("extern variable symbol %d has no symbol-table lookup (Emit was called without a symbol result, so an extern variable cannot be lowered to its real C name)", symbolID)
	}
	s, ok := st.symbols.Symbols.Symbol(symbolID)
	if !ok {
		return "", fmt.Errorf("extern variable symbol %d is not in the symbol table", symbolID)
	}
	return s.Name, nil
}

// collectReferencedExternData walks the entry body and every reachable helper
// body, returning the extern variables (externs, the unit's extern-binding
// declarations — the caller seeds it with exactly the ExternDeclaration nodes
// that describe data, i.e. have no reserved FunctionID) that are actually
// referenced there — as a SymbolValue read or as a Store/CompoundStore storage
// place — mapped to the declared type the reference carries. The walk is
// restricted to the reachable program so a forward declaration is emitted only
// for extern variables the emitted C actually uses. The declared type is taken
// from the reference node's own Type field (the checker sets a name reference's
// SymbolValue/StoragePlace type to the binding's declared type): the
// ExternDeclaration node itself carries no type, so the first reachable
// reference is the backend's only record of what the variable's type is.
func collectReferencedExternData(unit *tir.Unit, entryBlock tir.NodeID, helpers []helperInfo, externs map[symbol.SymbolID]struct{}) map[symbol.SymbolID]types.TypeID {
	referenced := make(map[symbol.SymbolID]types.TypeID)
	seen := make(map[tir.NodeID]bool)
	mark := func(symbolID symbol.SymbolID, typ types.TypeID) {
		if _, isExtern := externs[symbolID]; !isExtern {
			return
		}
		if _, recorded := referenced[symbolID]; !recorded {
			referenced[symbolID] = typ
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
			mark(node.Symbol, node.Type)
		case tir.Store, tir.CompoundStore:
			if len(node.Children) >= 1 {
				if place, ok := unit.Node(node.Children[0]); ok && place.Kind == tir.StoragePlace {
					mark(place.Symbol, place.Type)
				}
			}
		case tir.RecordConstruct:
			// Mirror collectReferencedGlobals: a struct construction's field
			// values live in node.Fields, not node.Children, so an extern
			// variable read used only as a field's construction value (e.g.
			// `lib::errno` in `Point.{ x = lib::errno }`) must be walked
			// explicitly or its declaration is never emitted.
			for _, field := range node.Fields {
				walk(field.Value)
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

// resolveExternDataInfo classifies one extern variable's declared type into the
// localInfo shape a local of the same type would carry, mirroring how
// resolveGlobalInfo classifies a mutable global's type. Every type the backend
// can declare as a local is classified here; buildExternDataDeclarations then
// rejects the aggregate/slice/optional/pointer/function shapes it cannot
// declare sensibly, never guessing.
func resolveExternDataInfo(unit *tir.Unit, snapshot *types.Snapshot, symbolID symbol.SymbolID, typ types.TypeID) (externDataInfo, error) {
	info, err := resolveTypedInfo(unit, snapshot, typ)
	if err != nil {
		return externDataInfo{typ: typ}, fmt.Errorf("extern variable symbol %d %v", symbolID, err)
	}
	return externDataInfo{typ: typ, info: info}, nil
}

// buildExternDataDeclarations returns the C forward-declaration text for every
// extern variable the current Emit references, one `extern <ctype> <name>;`
// per variable, in symbol-ID order (deterministic regardless of map iteration
// order). An extern variable's real storage is defined elsewhere (inside libc,
// or a C file the program links against), so the backend emits only this
// forward declaration — never a `static ... = ...;` definition — and every
// read/write reference then uses the variable's real C name, matching how the
// actual definition is spelled in its own translation unit. The declaration
// must follow the emitted typedefs (an enum-typed extern variable names its
// own enum typedef), which it does: it lands in the same file-scope region as
// buildGlobalStorage.
func buildExternDataDeclarations(st *emitState, unit *tir.Unit, snapshot *types.Snapshot) (string, error) {
	ids := make([]symbol.SymbolID, 0, len(st.externData))
	for id := range st.externData {
		ids = append(ids, id)
	}
	sort.Slice(ids, func(i, j int) bool { return ids[i] < ids[j] })
	lines := make([]string, 0, len(ids))
	for _, id := range ids {
		ctype, err := globalStorageCType(unit, snapshot, st.externData[id].info, "extern variable")
		if err != nil {
			return "", fmt.Errorf("extern variable symbol %d: %v", id, err)
		}
		lines = append(lines, fmt.Sprintf("extern %s %s;", ctype, st.externData[id].name))
	}
	return strings.Join(lines, "\n"), nil
}
