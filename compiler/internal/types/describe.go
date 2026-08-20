package types

import (
	"fmt"
	"strings"

	"github.com/pepplejoshua/pebble/compiler/internal/module"
	"github.com/pepplejoshua/pebble/compiler/internal/symbol"
)

// builtinName returns the short Pebble-source name for a builtin kind
// ("bool", "i32", "str", etc.), used in diagnostic messages.
func builtinName(k BuiltinKind) string {
	switch k {
	case Bool:
		return "bool"
	case Char:
		return "char"
	case Str:
		return "str"
	case Void:
		return "void"
	case Int:
		return "int"
	case Uint:
		return "uint"
	case I8:
		return "i8"
	case I16:
		return "i16"
	case I32:
		return "i32"
	case I64:
		return "i64"
	case U8:
		return "u8"
	case U16:
		return "u16"
	case U32:
		return "u32"
	case U64:
		return "u64"
	case F32:
		return "f32"
	case F64:
		return "f64"
	}
	return "<type>"
}

// DescribeKey returns a short human-readable name for a TypeKey, suitable for
// inclusion in diagnostic messages. It is the context-free fallback used when
// neither a snapshot (to resolve composite children) nor a symbol table (to
// resolve nominal/type-parameter names) is available; callers with access to
// either should use DescribeKeyResolved instead so real structure and
// declaration names appear.
func DescribeKey(key TypeKey) string {
	return describeKey(key, nil, nil)
}

// DescribeKeyResolved returns a short human-readable name for a TypeKey,
// recursing through composite children via lookup (which resolves a child
// TypeID back to its TypeKey, e.g. a *Snapshot or a *Store) and resolving
// nominal and type-parameter declarations through resolve (which maps a
// declaration SymbolID to its authored source name, e.g. "Color" or "Vec").
// Composite kinds recurse through their children using the same resolver. This
// is the printer every diagnostic and hover site with a snapshot and symbol
// table should use: it renders real structure and names, not coarse kind words.
func DescribeKeyResolved(key TypeKey, lookup func(TypeID) (TypeKey, bool), resolve func(symbol.SymbolID) string) string {
	return describeKey(key, lookup, resolve)
}

// LookupFromSnapshot builds a child-TypeID lookup closure from a *Snapshot,
// suitable for passing to DescribeKeyResolved. A nil snapshot returns nil,
// which makes composite children render as "<type>".
func LookupFromSnapshot(snap *Snapshot) func(TypeID) (TypeKey, bool) {
	if snap == nil {
		return nil
	}
	return func(id TypeID) (TypeKey, bool) { return snap.Key(id) }
}

// LookupFromStore builds a child-TypeID lookup closure from a *Store,
// suitable for passing to DescribeKeyResolved. A nil store returns nil, which
// makes composite children render as "<type>". Unlike LookupFromSnapshot it
// reads the live, mutable store rather than a frozen copy, so it works even
// when a full typed-IR unit was never built.
func LookupFromStore(store *Store) func(TypeID) (TypeKey, bool) {
	if store == nil {
		return nil
	}
	return func(id TypeID) (TypeKey, bool) { return store.Key(id) }
}

// ResolveFromResult builds a declaration-name resolver closure from a
// *symbol.Result, suitable for passing to DescribeKeyResolved. A nil result or
// a result without a symbol store returns nil, which makes nominal and
// type-parameter declarations render by identity.
func ResolveFromResult(resolution *symbol.Result) func(symbol.SymbolID) string {
	if resolution == nil || resolution.Symbols == nil {
		return nil
	}
	return func(id symbol.SymbolID) string {
		if s, ok := resolution.Symbols.Symbol(id); ok {
			return s.Name
		}
		return ""
	}
}

// QualifierMap builds the ModuleID-to-qualifier map the qualified resolver
// uses to prefix cross-module nominal type names, from a module's authored
// import edges. The qualifier for a target module is the one the importing
// module actually wrote for it (e.g. "set" for `import "std:set"`), recorded
// as ImportEdge.Qualifier by the module graph build.
func QualifierMap(imports []module.ImportEdge) map[module.ModuleID]string {
	if len(imports) == 0 {
		return nil
	}
	out := make(map[module.ModuleID]string, len(imports))
	for _, edge := range imports {
		if edge.Qualifier != "" {
			out[edge.Target] = edge.Qualifier
		}
	}
	return out
}

// ResolveFromResultQualified builds a declaration-name resolver closure for
// DescribeKeyResolved that qualifies nominal type names whose declaring module
// differs from the module a hover, inlay hint, or diagnostic is being rendered
// FOR. The qualifiers map is that current module's own import qualifiers
// (e.g. {"set" for the std:set module}), so a cross-module type renders as
// "set::Set[str]" while a type declared in the current module (or reachable
// only through a module the current file does not import) renders bare,
// exactly as the authored source reads. The common same-module case and the
// type-parameter/builtin cases are never qualified, so a nil or empty
// qualifiers map, a zero current module, or a plain same-module type all
// behave exactly like ResolveFromResult.
func ResolveFromResultQualified(resolution *symbol.Result, currentModule module.ModuleID, qualifiers map[module.ModuleID]string) func(symbol.SymbolID) string {
	if resolution == nil || resolution.Symbols == nil {
		return nil
	}
	if currentModule == 0 || len(qualifiers) == 0 {
		return ResolveFromResult(resolution)
	}
	return func(id symbol.SymbolID) string {
		s, ok := resolution.Symbols.Symbol(id)
		if !ok {
			return ""
		}
		if s.Module == currentModule {
			return s.Name
		}
		switch s.Kind {
		case symbol.SymbolType, symbol.SymbolExternType, symbol.SymbolRuntimeType:
		default:
			// Type parameters are local to their generic declaration and
			// builtins have no importable owning module; neither is qualified.
			return s.Name
		}
		if q := qualifiers[s.Module]; q != "" {
			return q + "::" + s.Name
		}
		return s.Name
	}
}

func describeKey(key TypeKey, lookup func(TypeID) (TypeKey, bool), resolve func(symbol.SymbolID) string) string {
	child := func(id TypeID) string {
		if lookup == nil {
			return "<type>"
		}
		ck, ok := lookup(id)
		if !ok {
			return "<type>"
		}
		return describeKey(ck, lookup, resolve)
	}
	if builtin, ok := key.Builtin(); ok {
		return builtinName(builtin)
	}
	switch key.kind {
	case Pointer:
		c, _ := key.Child()
		return "*" + child(c)
	case Optional:
		c, _ := key.Child()
		return "?" + child(c)
	case Slice:
		c, _ := key.Child()
		return "[]" + child(c)
	case Array:
		length, element, _ := key.Array()
		return fmt.Sprintf("[%d]%s", length, child(element))
	case Tuple:
		elements, _ := key.Elements()
		parts := make([]string, len(elements))
		for i, element := range elements {
			parts[i] = child(element)
		}
		return "(" + strings.Join(parts, ", ") + ")"
	case Function:
		_, parameters, result, _, _ := key.Function()
		params := make([]string, len(parameters))
		for i, parameter := range parameters {
			params[i] = child(parameter)
		}
		return "fn(" + strings.Join(params, ", ") + ") " + child(result)
	case Nominal:
		declaration, arguments, _ := key.Nominal()
		name := ""
		if resolve != nil {
			name = resolve(declaration)
		}
		if name == "" {
			name = fmt.Sprintf("type %d", declaration)
		}
		if len(arguments) > 0 {
			args := make([]string, len(arguments))
			for i, argument := range arguments {
				args[i] = child(argument)
			}
			return name + "[" + strings.Join(args, ", ") + "]"
		}
		return name
	case TypeParameter:
		declaration, _ := key.TypeParameter()
		if resolve != nil {
			if name := resolve(declaration); name != "" {
				return name
			}
		}
		return fmt.Sprintf("T%d", declaration)
	}
	return "<type>"
}
